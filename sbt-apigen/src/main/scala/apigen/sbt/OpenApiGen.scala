package apigen.sbt

import sbt._
import sbt.Keys._
import java.nio.file.{ Path, Paths }
import apigen._

object OpenApiGen extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    lazy val specFileInput: SettingKey[Path] = settingKey(
      "The path of directory where spec files live"
    )
    lazy val packageName: SettingKey[String] = settingKey("The full package name of generated code")
    lazy val moduleName: SettingKey[String] = settingKey(
      "The name of the module that contains generated code, it is generated as an object"
    )

    lazy val apiSrcGen: TaskKey[Seq[File]] = taskKey(
      "Generate source code and return a list of generated files"
    )
  }

  import autoImport._

  lazy val defaultSettings: Seq[Def.Setting[_]] = Seq(
    specFileInput := Paths.get("doc", "api"),
    packageName := "codegen",
    moduleName := "generated"
  )
  lazy val taskSettings: Seq[Def.Setting[_]] = Seq(
    apiSrcGen := Def.task {
      val outputDir = (sourceManaged.value / packageName.value).toPath
      openApiToCodeFiles(specFileInput.value.toFile, outputDir, packageName.value, moduleName.value)
        .unsafeRunSync()
        .toList
        .map(_.toFile)
    }.value
  )

  lazy val sourceGeneratorSettings: Seq[Def.Setting[_]] = Seq(
    // Register the muSrcGen task as a source generator.
    // If we don't do this, the compile task will not see the
    // generated files even if the user manually runs the muSrcGen task.
    sourceGenerators in Compile += (apiSrcGen in Compile).taskValue
  )
  override lazy val projectSettings: Seq[Def.Setting[_]] =
    defaultSettings ++ taskSettings ++ sourceGeneratorSettings
}
