ThisBuild / organization         := "io.github.fristi"
ThisBuild / organizationName     := "Fristi"
ThisBuild / licenses             := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / homepage             := Some(url("https://github.com/Fristi/apparatus"))
ThisBuild / developers           := List(
  Developer("Fristi", "Mark de Jong", "av3ng3r@gmail.com", url("https://github.com/Fristi"))
)
ThisBuild / versionScheme          := Some("early-semver")
ThisBuild / scmInfo                := Some(
  ScmInfo(url("https://github.com/Fristi/apparatus"), "scm:git@github.com:Fristi/apparatus.git")
)
ThisBuild / publishTo              := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (version.value.endsWith("-SNAPSHOT")) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

// BuildBuddy remote cache. The key comes from the environment (Coder injects it
// into the dev container) or from the sbt global base, and stays optional so the
// build still loads for anyone without an account.
lazy val buildBuddyApiKey: Option[String] = {
  val credentialFile = BuildPaths.defaultGlobalBase / "buildbuddy_credential.txt"
  val fromFile       =
    if (credentialFile.exists) Some(IO.read(credentialFile).stripPrefix("x-buildbuddy-api-key="))
    else None
  sys.env.get("BUILDBUDDY_API_KEY").orElse(fromFile).map(_.trim).filter(_.nonEmpty)
}

// Must match the Organization URL under BuildBuddy settings.
lazy val buildBuddyCacheUri = uri("grpcs://apparatus.buildbuddy.io")

Global / remoteCache        := buildBuddyApiKey.map(_ => buildBuddyCacheUri)
Global / remoteCacheHeaders ++= buildBuddyApiKey.map(key => s"x-buildbuddy-api-key=$key").toSeq

def commonSettings = Seq(
  scalaVersion := "3.8.3"
)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "apparatus-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"         % "2.13.0",
      "org.typelevel" %% "cats-effect"       % "3.5.4",
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
