ThisBuild / organization         := "io.github.fristi"
ThisBuild / organizationName     := "Fristi"
ThisBuild / licenses             := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / homepage             := Some(url("https://github.com/Fristi/apparatus"))
ThisBuild / developers           := List(
  Developer("Fristi", "Mark de Jong", "av3ng3r@gmail.com", url("https://github.com/Fristi"))
)
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"
ThisBuild / sonatypeRepository     := "https://central.sonatype.com/api/v1/publisher"
ThisBuild / publishTo              := sonatypePublishToBundle.value
ThisBuild / versionScheme          := Some("early-semver")
ThisBuild / scmInfo                := Some(
  ScmInfo(url("https://github.com/Fristi/apparatus"), "scm:git@github.com:Fristi/apparatus.git")
)

def commonSettings = Seq(
  scalaVersion := "3.8.3"
)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "apparatus-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"         % "2.13.0",
      "dev.zio"       %% "zio-blocks-schema" % "0.0.33"
    )
  )

lazy val doobie = (project in file("doobie"))
  .settings(commonSettings)
  .settings(
    name := "apparatus-doobie",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC12"
    )
  )
  .dependsOn(core)

lazy val examples = (project in file("examples"))
  .settings(commonSettings)
  .settings(
    name           := "apparatus-examples",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "kittens" % "3.5.0"
    )
  )
  .dependsOn(core, doobie)

lazy val tests = (project in file("tests"))
  .settings(commonSettings)
  .settings(
    name           := "apparatus-tests",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"                           % "1.3.0"  % Test,
      "com.dimafeng"  %% "testcontainers-scala-postgresql" % "0.44.1" % Test,
      "com.dimafeng"  %% "testcontainers-scala-munit"      % "0.44.1" % Test,
      "org.typelevel" %% "munit-cats-effect"               % "2.0.0"  % Test
    )
  )
  .dependsOn(examples % Test, doobie % Test)

lazy val docs = project
  .in(file("docs"))
  .enablePlugins(MdocPlugin)
  .dependsOn(core, doobie, examples)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    mdocIn         := (ThisBuild / baseDirectory).value / "docs-src",
    mdocOut        := baseDirectory.value,
    mdocVariables  := Map("VERSION" -> version.value)
  )

lazy val root = (project in file("."))
  .aggregate(core, doobie, examples, tests)
  .settings(
    publish / skip := true
  )
