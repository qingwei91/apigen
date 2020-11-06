import java.nio.file.Paths
import higherkindness.mu.rpc.srcgen.Model._

lazy val example = (project in file("."))
  .settings(name := "example")
  .enablePlugins(OpenApiGen)
  .settings(
    muSrcGenIdlType := IdlType.OpenAPI,
    scalaVersion := "2.12.10",
    specFileInput := Paths.get("doc", "api", "petstore.yaml"),
    openApiInputSpec := "petstore.yaml",
    openApiConfigFile := "config.yaml",
    openApiVerbose := Some(true),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-core"         % "0.21.2",
      "io.circe"   %% "circe-core"          % "0.12.3",
      "io.circe"   %% "circe-generic"       % "0.12.3",
      "org.http4s" %% "http4s-blaze-client" % "0.20.16",
      "org.http4s" %% "http4s-circe"        % "0.20.16"
    )
  )
