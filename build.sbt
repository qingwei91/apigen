import higherkindness.mu.rpc.srcgen.Model._

lazy val apigen = (project in file("."))
//  .dependsOn(
//    RootProject(uri("git://github.com/spartanz/schemaz#prototyping"))
//  )
  .settings(
    name := "apigen",
    version := "0.1",
    scalaVersion := "2.12.10",
    libraryDependencies ++= Seq(
      "org.openapitools"  % "openapi-generator"    % "4.3.0",
      "org.http4s"        %% "http4s-core"         % "0.21.2",
      "org.spartanz"      %% "schemaz-core"        % "0.1.0",
      "org.spartanz"      %% "schemaz-generic"     % "0.1.0",
      "io.higherkindness" %% "skeuomorph"          % "0.0.22",
      "io.circe"          %% "circe-core"          % "0.12.3",
      "io.circe"          %% "circe-generic"       % "0.12.3",
      "org.http4s"        %% "http4s-blaze-client" % "0.20.16",
      "org.http4s"        %% "http4s-circe"        % "0.20.16",
      "org.scalameta"     %% "scalameta"           % "4.3.8"
    ),
// Look for OpenAPI YAML files
    muSrcGenIdlType := IdlType.OpenAPI,
// Generate code that is compatible with http4s v0.20.x
    muSrcGenOpenApiHttpImpl := higherkindness.mu.rpc.srcgen.openapi.OpenApiSrcGenerator.HttpImpl.Http4sV20,
    addCompilerPlugin(
      ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
    )
  )
