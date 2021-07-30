Global / semanticdbEnabled := true

val commonSettings = Seq(
  organization := "io.github.maxkar",
  version := "0.0.1-SNAPSHOT",
  Compile / scalaSource := baseDirectory.value / "src",
  Test / scalaSource := baseDirectory.value / "src",
  scalaVersion := "3.0.1"
)

val libJsonClassic = project.in(file("json/classic"))
  .settings(commonSettings)
  .settings(
    name := "json-classic",
    description := 
      """Easy to use and lightweight json parsing/generation library.
      It provides javascript-like member access syntax and set of handy conversions.
      The library is Scala 3 version of lib-json library in Scala 2."""
  )

val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "corelib",
    description := 
      """An (opinionated) set of small modular (faceted) libraries for the most common tasks"""
  )
  .aggregate(libJsonClassic)
