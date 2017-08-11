name := "template-scala"
organization := "io.github.jeremyrsmith"
version := "0.1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-language:experimental.macros",
  "-language:higherKinds",
  "-deprecation"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)