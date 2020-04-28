import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, Paths, StandardOpenOption }

import cats.effect.IO
import higherkindness.skeuomorph.openapi.{ JsonSchemaF, ParseOpenApi }

import scala.meta.{ Source, Tree }

package object apigen {

  def openApiToCodeTree(
    inputFile: File,
    packageName: String,
    moduleName: String
  ): IO[Option[Source]] = {
    val specFile = ParseOpenApi.YamlSource(inputFile)

    ParseOpenApi
      .parseYamlOpenApi[IO, JsonSchemaF.Fixed]
      .parse(specFile)
      .map { apiSpec =>
        apiSpec.components.map(
          comps => SchemaBuilder.produceModelCode(comps.schemas, packageName, moduleName)
        )
      }
  }

  def openApiToCodeFiles(
    inputFile: File,
    outputPath: Path,
    packagePath: String,
    moduleName: String
  ): IO[Option[Path]] =
    openApiToCodeTree(inputFile, packagePath, moduleName).flatMap { maybeSource =>
      IO {
        maybeSource.map { fileSource =>
          Files.createDirectories(outputPath)
          val filePath = outputPath.resolve(Paths.get(s"$moduleName.scala"))
          filePath.toFile.createNewFile()
          Files.writeString(
            filePath,
            fileSource.syntax,
            StandardCharsets.UTF_8,
            StandardOpenOption.WRITE
          )
        }
      }
    }
}
