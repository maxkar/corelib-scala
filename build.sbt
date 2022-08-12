Global / semanticdbEnabled := true

Global / cancelable := true

val commonSettings = Seq(
  organization := "io.github.maxkar",
  version := "0.0.1-SNAPSHOT",
  Compile / scalaSource := baseDirectory.value / "src",
  Test / scalaSource := baseDirectory.value / "test",
  scalaVersion := "3.1.0"
)

val scalatest = "org.scalatest" %% "scalatest" % "3.2.9" % "test"


val libFun = project.in(file("fun"))
  .settings(commonSettings)
  .settings(
    name := "fun",
    description :=
      """Very general functional programming definitions.""",
    libraryDependencies += scalatest
  )


val libJsonClassic = project.in(file("json/classic"))
  .settings(commonSettings)
  .settings(
    name := "json-classic",
    description :=
      """Easy to use and lightweight json parsing/generation library.
      It provides javascript-like member access syntax and set of handy conversions.
      The library is Scala 3 version of lib-json library in Scala 2.""",
    libraryDependencies += scalatest
  )


val libJsonParser = project.in(file("json/parser"))
  .settings(commonSettings)
  .settings(
    name := "json-parser",
    description :=
      """Generic JSON parser. Provides the syntactic rules for parsing
      but abstracts from the rest of concepts like json model or input mechanism.""",
    libraryDependencies += scalatest
  ).dependsOn(
    libFun
  )


val libJsonParserChunky = project.in(file("json/parser-chunky"))
  .settings(commonSettings)
  .settings(
    name := "json-parser-chunky",
    description :=
      """An implementation of the parser that supports operations on partial input (
        i.e. "push" mode). This mode is useful for applications leveraging non-blocking
        input. The parser implementation is non-recursive and thus could work with high
        levels of json nesting.""",
    libraryDependencies += scalatest
  ).dependsOn(
    libJsonParser
  )


val libJsonAttributedModel = project.in(file("json/attributed/model"))
  .settings(commonSettings)
  .settings(
    name := "json-attributed-model",
    description :=
      """Json object model with support for non-json model attributes.
         | The attributes may define "auxilary" information like source location where element
         | was defined.
         |""".stripMargin
  )

val libJsonAttributedFactory = project.in(file("json/attributed/factory"))
  .settings(commonSettings)
  .settings(
    name := "json-attributed-factory",
    description := "Json model factories for use with the parsers provided by the platform.",
    libraryDependencies += scalatest
  )
  .dependsOn(libJsonParser, libJsonAttributedModel, libJsonParserChunky % "test")


val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "corelib",
    description :=
      """An (opinionated) set of small modular (faceted) libraries for the most common tasks"""
  )
  .aggregate(
    libFun,
    libJsonClassic, libJsonParser, libJsonParserChunky,
    libJsonAttributedModel, libJsonAttributedFactory
  )
