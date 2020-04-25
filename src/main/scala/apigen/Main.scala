package apigen

import java.io.File

import cats.effect.IO
import higherkindness.skeuomorph.openapi.{ JsonSchemaF, ParseOpenApi }
import higherkindness.droste._
import higherkindness.droste.data.Fix
import higherkindness.skeuomorph.openapi.schema.OpenApi

import scala.meta._
import apigen.core.SchemaBuilder._

object Main {

  def main(args: Array[String]): Unit = {

    val specFile = ParseOpenApi.YamlSource(
      new File(s"${System.getProperty("user.dir")}/example/petstore.yaml")
    )

    val result = ParseOpenApi
      .parseYamlOpenApi[IO, JsonSchemaF.Fixed]
      .parse(specFile)
      .unsafeRunSync()

    understand(result)
  }

  def understand(apiSpec: OpenApi[JsonSchemaF.Fixed]) =
    apiSpec.components.map { comps =>
      comps.schemas.map {
        case (objKey, jsonSchema) =>
          val collapse: Fix[JsonSchemaF] => TreeBuilder = scheme.cata(printCode)
          val (typeRegistry, finalTypeRef) =
            collapse(jsonSchema)(TypeAlias(objKey)).run(Map.empty).value
          println(finalTypeRef)

          typeRegistry.foreach {
            case (tpe, tree) =>
              println(tpe)
              println(tree.syntax)
          }
      }
    }

}
