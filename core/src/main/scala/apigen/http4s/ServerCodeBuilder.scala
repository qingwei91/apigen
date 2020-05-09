package apigen.http4s

import apigen._
import cats.data.{ NonEmptyList, State }
import cats.syntax.all._
import cats.instances.list._
import cats.instances.option._
import higherkindness.skeuomorph.openapi.schema._

import scala.meta._
import apigen.Utils._

class ServerCodeBuilder(components: Components[TypeProducer]) {
  type A               = TypeProducer
  type TypeDefRegistry = Map[Type.Name, NonEmptyList[Defn]]

  def pathToTypeName(path: String): String =
    path.split("/").map(_.capitalize).mkString("")

  def methodNodeToParamName(methodNode: MethodNode): String = {
    val pathTypeName = pathToTypeName(
      methodNode.parent.name.replaceAll("-", "").replaceAll("_", "")
    )

    val lower1stChar = methodNode.name.charAt(0).toLower
    val methodName   = s"${lower1stChar}${methodNode.name.drop(1)}"
    s"${methodName}$pathTypeName"
  }

  def methodNodeToTypeName(methodNode: MethodNode): String =
    methodNodeToParamName(methodNode).capitalize

  /**
   * An operation produces
   *  - 0 or more models (by schema)
   *  - 1 abstract function to be implemented by user that represent the operation
   *  - 1 http route implementation that depends on the abstract function, the implementation need to extract http level params
   *    into scala and invoke the abstract operation function
   */
  type OperationFn = Decl.Def
  type Http4sRoute = OperationFn => Defn.Def

  val jsonMediaType = "application/json"

  def fail(msg: String) = throw new Error(msg)

  def reExtendClsDef(clsDef: Defn.Class, baseTrait: Template): Defn.Class = {
    import clsDef._

    q"..${mods} class ${name}[..${tparams}] ..${ctor.mods} (...${ctor.paramss}) extends $baseTrait"
  }

  def operationCode(op: Path.Operation[A])(
    parent: MethodNode
  ): CodeProducer[OperationFn] = {
    // produce
    val opFnParams: CodeProducer[List[Term.Param]] = deferenceParams(op.parameters)
      .traverse { param =>
        openApiParamToMethodParam(param)(PathParam(parent, param.name, param.in).asLeft)
      }
    val reqBodyParam: CodeProducer[Option[Term.Param]] = op.requestBody
      .map(_.fold(identity, dereferenceRequestBody))
      .traverse(req => requestBodyParam(req)(parent))
      .map(_.flatten)

    val responseType: CodeProducer[Option[Type.Name]] =
      responsesType(dereferenceResponses(op.responses))(parent)
    val methodName = Term.Name(op.operationId.getOrElse(methodNodeToParamName(parent)))
    val methodParams = for {
      paramsParams <- opFnParams
      bodyParam    <- reqBodyParam
    } yield {
      bodyParam.toList ::: paramsParams
    }
    val methodReturnType = responseType.map {
      case Some(value) => value
      case None        => t"Unit"
    }
    for {
      paramList  <- methodParams
      returnType <- methodReturnType
    } yield {
      q"def $methodName(..$paramList): $returnType"
    }
  }

  def requestBodyParam(
    requestBody: Request[A]
  )(parent: MethodNode): CodeProducer[Option[Term.Param]] = {
    val isRequired = requestBody.required
    val jsonReqSchema = requestBody.content
      .map {
        case (k, v) => k.toLowerCase -> v
      }
      .get(jsonMediaType) // todo: only handle `application/json` for now
      .flatMap(_.schema)  // todo: ignore encoding for now

    val paramName = Term.Name(methodNodeToParamName(parent))

    jsonReqSchema.traverse[CodeProducer, Term.Param] { typeProducer =>
      typeProducer(PathRequest(parent)).map {
        case t"Option[$inner]" =>
          fail("Should not happen, schema should never be Optional on its own")
        case inner if isRequired => param"$paramName: $inner"
        case inner               => param"$paramName: Option[$inner]"
      }
    }
  }

  def responsesType(
    responses: Map[String, Response[A]]
  )(parent: MethodNode): CodeProducer[Option[Type.Name]] = {
    responses.size match {
      case 0 => State.pure(None)
      case 1 =>
        val (responseCode, response) = responses.head
        singleResponseTpe(responseCode, response)(parent).map(_.map(_.name))
      case n =>
        val adtName = Type.Name(s"${methodNodeToTypeName(parent)}Response")
        val adt     = q"sealed trait $adtName"

        val responsesClsDef = responses.toList
          .traverse {
            case (responseCode, response) =>
              singleResponseTpe(responseCode, response)(parent)
          }
          .map { responseClsDefs =>
            responseClsDefs.collect {
              case Some(v) => v
            }
          }

        for {
          _    <- addTypeDef(adt.name, adt)
          defs <- responsesClsDef
          _ <- defs.traverse[CodeProducer, Unit] { clsDef =>
                val updatedClsDef = reExtendClsDef(clsDef, adt.templ)
                addTypeDef(clsDef.name, updatedClsDef)
              }
        } yield {
          Some(adtName)
        }
    }
  }

  def typifyResponseCode(r: String): Either[Int, DefaultCode.type] = {
    if (r.toLowerCase() == "default") {
      Right(DefaultCode)
    } else {
      Left(r.toInt)
    }
  }

  def singleResponseTpe(responseCode: String, response: Response[A])(
    parent: MethodNode
  ): CodeProducer[Option[Defn.Class]] = {
    // the result type should be a product of content and headers
    val jsonResponseCode: CodeProducer[Option[Type]] =
      response.content
        .get(jsonMediaType)
        .flatMap(_.schema)
        .traverse { typeProducer =>
          val responseNode     = PathResponse(parent, typifyResponseCode(responseCode))
          val jsonMediaTpeNode = MediaTypePath(responseNode, jsonMediaType)
          typeProducer(jsonMediaTpeNode)
        }

    val headerType: CodeProducer[Option[Type]] = headersType(dereferenceHeaders(response.headers))(
      PathResponse(parent, typifyResponseCode(responseCode))
    )

    /*
    ensure we always return a case class, this increase clarity and also allow caller to change them into ADT
    without inverting control
     */
    (jsonResponseCode, headerType).tupled.flatMap {
      case (Some(responseBody), Some(headerType)) =>
        val clsName = Type.Name(s"${methodNodeToTypeName(parent)}$responseCode")
        val clsDef  = q"case class $clsName(body: $responseBody, headers: $headerType)"
        addTypeDef(clsName, clsDef).map(_ => Some(clsDef))
      case (Some(responseBody), None) =>
        val clsName  = Type.Name(s"${methodNodeToTypeName(parent)}$responseCode")
        val classDef = q"case class $clsName(body: $responseBody)"
        addTypeDef(clsName, classDef).map(_ => Some(classDef))

      case (None, Some(headerType)) =>
        val clsName = Type.Name(s"${methodNodeToTypeName(parent)}$responseCode")
        val clsDef  = q"case class $clsName(headers: $headerType)"
        addTypeDef(clsName, clsDef).map(_ => Some(clsDef))

      case (None, None) => State.pure(None)
    }

  }

  def singleHeaderType(headerName: String, header: Header[A])(parent: HeaderParent): ModelCode = {
    header.schema(HeaderPath(parent, headerName))
  }

  def headerProductTypeName(headerParent: HeaderParent): String = {
    headerParent match {
      case PathResponse(parent, responseCode) =>
        s"${methodNodeToTypeName(parent)}${responseCode.fold(_.toString, _ => "Default")}Header"
      case ComponentResponse(name) => name.capitalize
    }
  }

  def headersType(
    headers: Map[String, Header[A]]
  )(parent: HeaderParent): CodeProducer[Option[Type]] = {

    headers.size match {
      case 0 => State.pure(None)
      case 1 =>
        val (name, header) = headers.head
        singleHeaderType(name, header)(parent).map(Some(_))
      case n =>
        val headerParams: CodeProducer[List[Term.Param]] =
          headers.toList.traverse[CodeProducer, Term.Param] {
            case (name, header) =>
              singleHeaderType(name, header)(parent).map { tpe =>
                val paramName = Term.Name(name)
                param"$paramName: $tpe"
              }
          }
        val clsName = Type.Name(headerProductTypeName(parent))
        for {
          params           <- headerParams
          headerProductDef = q"case class $clsName(..$params)"
          _                <- addTypeDef(clsName, headerProductDef)
        } yield {
          Some(clsName)
        }
    }
  }

  def dereferenceResponses(
    responses: Map[String, Either[Response[A], Reference]]
  ): Map[String, Response[A]] = {
    responses.mapValues { resOrRef =>
      resOrRef.fold(identity, dereferenceResponse)
    }
  }

  def dereferenceResponse(responseRef: Reference): Response[A] = {
    // todo: fix the bug of referencing, we need to use regex to extract the right key
    // it should be `key`, not `#/components/responses/key`
    components.responses
      .getOrElse(responseRef.ref, throw new Error(s"Reference $responseRef not found")) match {
      case Left(param) => param
      case Right(ref)  => dereferenceResponse(ref)
    }
  }

  def normalizeHeader(headerName: String): String = {
    headerName.split("-").map(_.capitalize).mkString("")
  }

  def dereferenceHeaders[T](
    headers: Map[String, Either[Header[T], Reference]]
  ): Map[String, Header[T]] = {
    headers.mapValues(_.left.getOrElse(throw new Error("Cannot handle referenced Header yet!")))
  }

  def openApiParamToMethodParam(
    parameter: Parameter[A]
  )(context: Either[PathParam, ComponentParam]): CodeProducer[Term.Param] = {
    val name     = Term.Name(parameter.name)
    val tpeState = parameter.schema(context.merge)
    tpeState.map(tpe => param"$name: $tpe")
  }

  /*
  TODO: this method does not handle inifinite recursion
   */
  def deferenceParam(
    paramRef: Reference
  ): Parameter[A] = {
    // todo: bug, we need to pattern match ref and extract the last bit, it currently looks like #/components/parameters/Meh
    // we only need the `Meh` bit
    components.parameters
      .getOrElse(paramRef.ref, fail(s"Reference $paramRef not found")) match {
      case Left(param) => param
      case Right(ref)  => deferenceParam(ref)
    }
  }

  def deferenceParams(
    params: List[Either[Parameter[A], Reference]]
  ): List[Parameter[A]] = {
    params.map(_.fold(identity, deferenceParam))
  }

  def dereferenceRequestBody(reqBody: Reference): Request[A] = {
    components.requestBodies.getOrElse(reqBody.ref, fail(s"Reference $reqBody not found")) match {
      case Left(req)  => req
      case Right(ref) => dereferenceRequestBody(ref)
    }

  }
}
