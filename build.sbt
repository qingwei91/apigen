lazy val apigen = (project in file("."))
  .aggregate(core, sbtApiGen)
  .dependsOn(core, sbtApiGen)

lazy val core = (project in file("core"))
  .settings(
    name := "apigen-core",
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
    addCompilerPlugin(
      ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
    )
  )

lazy val sbtApiGen = (project in file("sbt-apigen"))
  .dependsOn(core)
  .enablePlugins(SbtPlugin)
  .settings(name := "sbt-apigen")
  .settings(
    scalaVersion := "2.12.10",
    sbtPlugin := true
  )
