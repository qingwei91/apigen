package apigen

import java.io.File

import cats.effect.IO
import higherkindness.skeuomorph.openapi.{ JsonSchemaF, ParseOpenApi }
import higherkindness.skeuomorph.openapi.schema.OpenApi

import scala.meta._

object Main {

  def main(args: Array[String]): Unit = {

    val specFile = ParseOpenApi.YamlSource(
      new File(s"${System.getProperty("user.dir")}/example/petstore.yaml")
    )

    val result = ParseOpenApi
      .parseYamlOpenApi[IO, JsonSchemaF.Fixed]
      .parse(specFile)
      .unsafeRunSync()
  }

}
