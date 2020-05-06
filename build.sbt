// Versions
val catsV = "2.0.0"
val drosteV = "0.8.0"
val scalaTestV = "3.1.0"

lazy val scala212 = "2.12.10"

lazy val supportedScalaVersions = List(scala212)

// Settings
lazy val commonSettings = Seq(
  name := "datum",
  version := "0.5.2-SNAPSHOT", //not published
  scalaVersion := scala212,
  crossScalaVersions := supportedScalaVersions,
  organization := "io.github.voltir",
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  scalacOptions ++= Seq(
    "-Xlint",
    "-deprecation",
    "-unchecked",
    "-Ypartial-unification",
    "-Ypatmat-exhaust-depth",
    "40",
    "-Ywarn-dead-code",
    "-Ywarn-infer-any",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
    "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates" // Warn if a private member is unused.
    //"-Xfatal-warnings"
  ),
  fork in Test := true,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsV,
    "org.typelevel" %% "alleycats-core" % catsV,
    "io.higherkindness" %% "droste-core" % drosteV,
    "com.lihaoyi" %% "pprint" % "0.5.3" % Test,
    "org.scalatest" %% "scalatest" % scalaTestV % Test,
    "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
  )
)

// Modules
lazy val datum = (project in file("."))
  .settings(
    skip in publish := true,
    // must be set to nil to avoid double publishing
    crossScalaVersions := Nil,
    sonatypeProfileName := "io.github.voltir"
  )
  .aggregate(core, gen, testCore, ujson, avro)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "datum-core",
  )
  .settings(sonatypePublish)

lazy val gen = (project in file("gen"))
  .settings(commonSettings)
  .settings(
    name := "datum-gen",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestV,
      "io.chrisdavenport" %% "cats-scalacheck" % "0.2.0",
    )
  )
  .settings(sonatypePublish)
  .dependsOn(core)

lazy val avro = (project in file("avro"))
  .settings(commonSettings)
  .settings(
    name := "datum-avro",
    libraryDependencies ++= Seq(
      "org.apache.avro" % "avro" % "1.9.0",
      "org.xerial.snappy" % "snappy-java" % "1.1.7.3" % Test
    )
  )
  .settings(sonatypePublish)
  .dependsOn(core, gen % Test)

// sub project so that test code can depend on both core and gen
// otherwise there is a circular dependency
lazy val testCore = (project in file("test-core"))
  .settings(commonSettings)
  .settings(
    skip in publish := true,
    name := "datum-core-test"
  )
  .dependsOn(core, gen % Test)

lazy val ujson = (project in file("ujson"))
  .settings(commonSettings)
  .settings(
    name := "datum-ujson",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "0.9.5"
    )
  )
  .settings(sonatypePublish)
  .dependsOn(core, gen % Test)

// Publish Settings
val sonatypePublish = Seq(
  //releaseCrossBuild := true,
  publishMavenStyle := true,
  publishArtifact.in(Test) := false,
  pomIncludeRepository := Function.const(false),
  homepage := Some(url("https://github.com/Voltir/datum")),
  sonatypeProfileName := "io.github.voltir",
  licenses += ("MIT license", url("http://www.opensource.org/licenses/mit-license.php")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/Voltir/datum"),
      "scm:git:git@github.com:Voltir/datum.git"
    )),
  developers := List(
    Developer(
      "voltir",
      "Nick Childers",
      "voltir42@gmail.com",
      url("https://github.com/Voltir")
    )
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
)
