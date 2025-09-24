name := "advent-of-code-2020"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.2.0",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)
