package apigen

import java.io.File

import apigen.Utils.{ ModelCode, TypeProducer }
import apigen.http4s.ServerCodeBuilder
import cats.effect.IO
import cats.Functor
import cats.syntax.all._
import higherkindness.droste._
import higherkindness.skeuomorph.openapi._
import higherkindness.skeuomorph.openapi.schema
import higherkindness.droste.Basis._
import higherkindness.skeuomorph.openapi.print.PackageName
import higherkindness.skeuomorph.openapi.schema.{
  Callback,
  Components,
  Encoding,
  Header,
  MediaType,
  Parameter,
  Path,
  Reference,
  Request,
  Response
}

import scala.meta._

object Main {

  def main(args: Array[String]): Unit = {

    val specFile = ParseOpenApi.YamlSource(
      new File(s"${System.getProperty("user.dir")}/example/doc/api/petstore.yaml")
    )

    val result = ParseOpenApi
      .parseYamlOpenApi[IO, JsonSchemaF.Fixed]
      .parse(specFile)
      .unsafeRunSync()

    val flattenSchema = schema.OpenApi.extractNestedTypes(result)
    import client.http4s.circe._
    import client.http4s.print._
    implicit val http4ss = v20.v20Http4sSpecifics
    val str              = impl[JsonSchemaF.Fixed].print(PackageName("apigen") -> flattenSchema)

    val components = flattenSchema.components.get

    def unexpectedPosition(position: SchemaPosition) =
      throw new Error(s"Schema appears at unexpected position $position")

    def urlToName(url: String): String = {
      url
        .replaceAll("-", "/")
        .replaceAll("_", "/")
        .split("/")
        .map(_.capitalize)
        .mkString("")
    }

    def methodNodeToName(methodNode: MethodNode): String = {
      val pathName   = urlToName(methodNode.parent.name)
      val methodName = methodNode.name.capitalize
      s"$methodName$pathName"
    }

    val schemaToTypeProducer: JsonSchemaF.Fixed => TypeProducer = { schema =>
      {
        case a @ ApiPath(name)            => unexpectedPosition(a)
        case m @ MethodNode(parent, name) => unexpectedPosition(m)
        case parent: MediaTypeParent      => unexpectedPosition(parent)
        case parent: HeaderParent         => unexpectedPosition(parent)
        case PathParam(parent, paramName, location) =>
          val methodName = methodNodeToName(parent)
          val schemaName = s"$methodName$paramName${location.value}"
          SchemaBuilder.modelCode(schemaName, schema)
        case HeaderPath(responseParent, headerName) =>
          responseParent match {
            case ComponentResponse(name) => SchemaBuilder.modelCode(name, schema)
            case PathResponse(methodParent, responseCode) =>
              val methodName = methodNodeToName(methodParent)
              val schemaName = s"$methodName$headerName"
              SchemaBuilder.modelCode(schemaName, schema)
          }
        case MediaTypePath(mediaParent, mediaType) =>
          mediaParent match {
            case ComponentResponse(name) => SchemaBuilder.modelCode(name, schema)
            case ComponentRequest(name)  => SchemaBuilder.modelCode(name, schema)
            case PathRequest(parent) =>
              val schemaName = s"${methodNodeToName(parent)}${mediaType}Request"
              SchemaBuilder.modelCode(schemaName, schema)
            case PathResponse(parent, responseCode) =>
              val schemaName = s"${methodNodeToName(parent)}${mediaType}Response"
              SchemaBuilder.modelCode(schemaName, schema)
          }
        case Schema(name)                              => SchemaBuilder.modelCode(name, schema)
        case ComponentParam(name, paramName, location) => SchemaBuilder.modelCode(name, schema)
      }
    }

    val builder          = new ServerCodeBuilder(components.map(schemaToTypeProducer))
    val (path1, pathObj) = flattenSchema.paths.head

    val myRe = builder.operationCode(
      pathObj.map(schemaToTypeProducer).get.get
    )(MethodNode(ApiPath(path1), "get"))

    val (defs, finalType) = myRe.run(Map.empty).value

    println(defs)
    println(finalType)
  }
  implicit val mediaFunctor: Functor[MediaType] = new Functor[MediaType] {
    override def map[A, B](fa: MediaType[A])(f: A => B): MediaType[B] = {
      fa.copy(schema = fa.schema.map(f), encoding = Map.empty)
    }
  }
  implicit val headerFunctor: Functor[Header] = new Functor[Header] {
    override def map[A, B](fa: Header[A])(f: A => B): Header[B] = {
      fa.copy(schema = f(fa.schema))
    }
  }

  implicit val requestFunctor: Functor[Request] = new Functor[Request] {
    override def map[A, B](fa: Request[A])(f: A => B): Request[B] = {
      fa.copy(content = fa.content.mapValues(_.map(f)))
    }
  }
  implicit val responseFunctor: Functor[Response] = new Functor[Response] {
    override def map[A, B](fa: Response[A])(f: A => B): Response[B] = {
      val headerB = fa.headers.mapValues {
        case Left(value)  => Left(value.map(f))
        case Right(value) => Right(value)
      }
      fa.copy(content = fa.content.mapValues(_.map(f)), headers = headerB)
    }
  }
  implicit val paramFunctor: Functor[Parameter] = new Functor[Parameter] {
    override def map[A, B](fa: Parameter[A])(f: A => B): Parameter[B] = {
      fa match {
        case p: Parameter.Path[A]   => p.copy(schema = f(fa.schema))
        case q: Parameter.Query[A]  => q.copy(schema = f(fa.schema))
        case h: Parameter.Header[A] => h.copy(schema = f(fa.schema))
        case c: Parameter.Cookie[A] => c.copy(schema = f(fa.schema))

      }
    }
  }
  implicit val componentFunctor: Functor[Components] = new Functor[Components] {
    override def map[A, B](fa: Components[A])(f: A => B): Components[B] = {
      val schemasB  = fa.schemas.mapValues(f)
      val responseB = fa.responses.mapValues(_.left.map(_.map(f)))
      val paramsB   = fa.parameters.mapValues(_.left.map(_.map(f)))
      val requestsB = fa.requestBodies.mapValues(_.left.map(_.map(f)))
      fa.copy(
        schemas = schemasB,
        responses = responseB,
        parameters = paramsB,
        requestBodies = requestsB
      )
    }
  }

  implicit val operationFunctor: Functor[Path.Operation] = new Functor[Path.Operation] {
    override def map[A, B](fa: Path.Operation[A])(f: A => B): Path.Operation[B] = {
      val paramsB    = fa.parameters.map(_.left.map(_.map(f)))
      val reqsB      = fa.requestBody.map(_.left.map(_.map(f)))
      val responseB  = fa.responses.mapValues(_.left.map(_.map(f)))
      val callbacksB = Map.empty[String, Either[Callback[B], Reference]]
      fa.copy(
        parameters = paramsB,
        requestBody = reqsB,
        responses = responseB,
        callbacks = callbacksB
      )
    }
  }
  implicit val pathItem: Functor[schema.Path.ItemObject] = new Functor[Path.ItemObject] {
    override def map[A, B](fa: Path.ItemObject[A])(f: A => B): Path.ItemObject[B] = {
      fa.copy(
        get = fa.get.map(_.map(f)),
        put = fa.put.map(_.map(f)),
        post = fa.post.map(_.map(f)),
        delete = fa.delete.map(_.map(f)),
        options = fa.options.map(_.map(f)),
        head = fa.head.map(_.map(f)),
        patch = fa.patch.map(_.map(f)),
        trace = fa.trace.map(_.map(f)),
        parameters = fa.parameters.map(_.left.map(_.map(f)))
      )
    }
  }
}
