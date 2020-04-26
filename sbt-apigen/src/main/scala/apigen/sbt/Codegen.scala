package apigen.sbt

import sbt._
import sbt.Keys._
import java.nio.file.{ Path, Paths }
import apigen._

object Codegen extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  object autoimport {
    lazy val specFileInput: SettingKey[Path] = settingKey(
      "The path of directory where spec files live"
    )
    lazy val codeOutput: SettingKey[Path] = settingKey("The path of directory to output code")

    lazy val srcGen: TaskKey[Seq[File]] = taskKey(
      "Generate source code and return a list of generated files"
    )
  }

  import autoimport._

  lazy val defaultSettings: Seq[Def.Setting[_]] = Seq(
    specFileInput := Paths.get("doc", "api"),
    codeOutput := (sourceManaged.value / "codegen").toPath
  )
  lazy val taskSettings: Seq[Def.Setting[_]] = Seq(
    srcGen := Def
      .task(
        openApiToCodeFiles(specFileInput.value.toFile, codeOutput.value)
          .unsafeRunSync()
          .map(_.toFile)
      )
      .value
  )
}
