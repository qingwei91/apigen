import java.nio.file.Paths

lazy val example = (project in file("."))
  .settings(name := "example")
  .enablePlugins(OpenApiGen)
  .settings(
    scalaVersion := "2.12.10",
    specFileInput := Paths.get("doc", "api", "petstore.yaml")
  )
