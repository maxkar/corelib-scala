Global / semanticdbEnabled := true

Global / cancelable := true

val commonSettings = Seq(
  organization := "io.github.maxkar",
  version := "0.0.1-SNAPSHOT",
  Compile / scalaSource := baseDirectory.value / "src",
  Test / scalaSource := baseDirectory.value / "test",
  Test / fork := true,
  scalaVersion := "3.2.0",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oS"),
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


val libJsonParser2 = project.in(file("json/parser2"))
  .settings(commonSettings)
  .settings(
    name := "json-parser2",
    description :=
      """Generic JSON parser (second version). Provides the syntactic rules for parsing
      but abstracts from the rest of concepts like json model or input mechanism.""",
    libraryDependencies += scalatest,
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


val libJsonWriter = project.in(file("json/writer"))
  .settings(commonSettings)
  .settings(
    name := "json-writer",
    description := "Utilities for writing/outputting json (both ugly and pretty print)"
  )


val libJsonQuery = project.in(file("json/query"))
  .settings(commonSettings)
  .settings(
    name := "json-query",
    description :=
        """Json navigation and query library. It provides an ability to navigate abstract DOM and
          | capture paths during that navigation. The paths may later be used to provide some good context
          | (for example, in error messages).
          |
          |The library only defines generic tools (queries and query integration interfaces), the actual
          | integration with Json Model is responsibility of that particular model.""".stripMargin,
    libraryDependencies += scalatest
  )


val libJsonSimpleModel = project.in(file("json/simple/model"))
  .settings(commonSettings)
  .settings(
    name := "json-simple-model",
    description :=
      """Very basic json object model that is "just a tree"."""
  )

val libJsonSimpleFactory = project.in(file("json/simple/factory"))
  .settings(commonSettings)
  .settings(
    name := "json-simple-factory",
    description := "Json model factories for use with the parsers provided by the platform.",
    libraryDependencies += scalatest
  )
  .dependsOn(libJsonParser, libJsonSimpleModel, libJsonParserChunky % "test")


val libJsonSimpleWriter = project.in(file("json/simple/writer"))
  .settings(commonSettings)
  .settings(
    name := "json-simple-writer",
    description := "Bindings to the JSON writer for the simple model.",
    libraryDependencies += scalatest
  )
  .dependsOn(libJsonWriter, libJsonSimpleModel)


val libJsonSimpleQuery = project.in(file("json/simple/query"))
  .settings(commonSettings)
  .settings(
    name := "json-simple-query",
    description := "Query integration for the simple JSON",
    libraryDependencies += scalatest
  )
  .dependsOn(libJsonSimpleModel, libJsonQuery, libFun)


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


val libJsonAttributedWriter = project.in(file("json/attributed/writer"))
  .settings(commonSettings)
  .settings(
    name := "json-attributed-writer",
    description := "Bindings to the JSON writer for the attributed model.",
    libraryDependencies += scalatest
  )
  .dependsOn(libJsonWriter, libJsonAttributedModel)


val libJsonAttributedQuery = project.in(file("json/attributed/query"))
  .settings(commonSettings)
  .settings(
    name := "json-attributed-query",
    description := "Query integration for the attributed JSON",
    libraryDependencies += scalatest
  )
  .dependsOn(
    libJsonAttributedModel, libJsonQuery, libFun,
    libJsonAttributedFactory % Test, libJsonParserChunky % Test
  )


val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "corelib",
    description :=
      """An (opinionated) set of small modular (faceted) libraries for the most common tasks"""
  )
  .aggregate(
    libFun,
    libJsonClassic,
    libJsonParser, libJsonParser2, libJsonParserChunky,
    libJsonWriter, libJsonQuery,
    libJsonSimpleModel, libJsonSimpleFactory, libJsonSimpleWriter, libJsonSimpleQuery,
    libJsonAttributedModel, libJsonAttributedFactory, libJsonAttributedWriter, libJsonAttributedQuery,
  )
