package apigen

import java.io.File

import apigen.Utils.TypeProducer
import apigen.http4s.ServerCodeBuilder
import apigen.implicits._
import cats.effect.IO
import cats.syntax.all._
import higherkindness.droste.Basis._
import higherkindness.skeuomorph.openapi.print.PackageName
import higherkindness.skeuomorph.openapi.{ schema, print => modelPrint, _ }

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
    import client.http4s.{ print => clientPrint }
    implicit val http4ss = clientPrint.v20.v20Http4sSpecifics
    val clientImplCode =
      clientPrint.impl[JsonSchemaF.Fixed].print(PackageName("apigen") -> flattenSchema)

    val modelCode = modelPrint.model[JsonSchemaF.Fixed].print(flattenSchema)
    println(modelCode)

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

//    println(defs)
//    println(finalType)

  }

}
