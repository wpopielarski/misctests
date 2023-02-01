name := "Game Server"

version := "1.0"

scalaVersion := "2.13.6" // Fixed - https://docs.codescreen.com/#/creating-custom-scala-assessments

val http4sVersion = "0.23.17"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.4.1",
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-ember-client" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.14.3",
  // Optional for string interpolation to JSON model
  "io.circe" %% "circe-literal" % "0.14.3",
  "org.typelevel" %% "cats-effect-testing-scalatest" % "1.4.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.14" % Test,
)

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

//addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
