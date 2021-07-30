Global / semanticdbEnabled := true

val commonSettings = Seq(
  organization := "io.github.maxkar",
  version := "0.0.1-SNAPSHOT",
  Compile / scalaSource := baseDirectory.value / "src",
  Test / scalaSource := baseDirectory.value / "src",
  scalaVersion := "3.0.1"
)

val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "corelib",
    description := 
      """An (opinionated) set of small modular (faceted) libraries for the most common tasks"""
  )
