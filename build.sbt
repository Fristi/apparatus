ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.3"

lazy val core = (project in file("core"))
  .settings(
    name := "apparatus-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"        % "2.13.0",
      "dev.zio"       %% "zio-blocks-schema" % "0.0.33",
      "org.scalameta" %% "munit"            % "1.3.0" % Test
    )
  )

lazy val root = (project in file("."))
  .aggregate(core)
