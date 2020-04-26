import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, Paths, StandardOpenOption }

import cats.effect.IO
import higherkindness.skeuomorph.openapi.{ JsonSchemaF, ParseOpenApi }

import scala.meta.Tree

package object apigen {

  def openApiToCodeTree(inputFile: File): IO[Map[String, Tree]] = {
    val specFile = ParseOpenApi.YamlSource(inputFile)

    ParseOpenApi
      .parseYamlOpenApi[IO, JsonSchemaF.Fixed]
      .parse(specFile)
      .map { apiSpec =>
        apiSpec.components.map(comps => SchemaBuilder.produceModelCode(comps.schemas))
      }
      .map(_.getOrElse(Map.empty))
  }

  def openApiToCodeFiles(inputFile: File, outputPath: Path): IO[List[Path]] =
    openApiToCodeTree(inputFile).flatMap { fileCodeMap =>
      IO {
        Files.createDirectory(outputPath)
        fileCodeMap.map {
          case (fileName, tree) =>
            val filePath = Files.createFile(outputPath.relativize(Paths.get(fileName)))
            Files.writeString(
              filePath,
              tree.syntax,
              StandardCharsets.UTF_8,
              StandardOpenOption.WRITE
            )
        }.toList
      }
    }
}
