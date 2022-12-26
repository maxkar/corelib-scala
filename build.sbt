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

val appSettings = Seq(
  run / fork := true,
  run / connectInput := true,
  outputStrategy := Some(StdoutOutput),
)

val scalatest = "org.scalatest" %% "scalatest" % "3.2.9" % "test"

val jettyVersion = "9.4.49.v20220914"


val libFun = project.in(file("fun"))
  .settings(commonSettings)
  .settings(
    name := "fun",
    description :=
      """Very general functional programming definitions.""",
    libraryDependencies += scalatest
  )


val libText = project.in(file("text"))
  .settings(commonSettings)
  .settings(
    name := "text",
    description := "Generic text utilities (like text input/output, text location representation, etc...).",
    libraryDependencies += scalatest,
  ).dependsOn(
    libFun
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
    libraryDependencies += scalatest,
  ).dependsOn(
    libFun,
    libText,
  )


val libJsonWriter = project.in(file("json/writer"))
  .settings(commonSettings)
  .settings(
    name := "json-writer",
    description := "Utilities for writing/outputting json (both ugly and pretty print)",
    libraryDependencies += scalatest,
  )
  .dependsOn(
    libFun, libText
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


val libJsonSimple = project.in(file("json/simple"))
  .settings(commonSettings)
  .settings(
    name := "json-simple",
    description :=
      """Simple json model (natural JSON tree) with corresponding input,
         |output and query facilities""".stripMargin,
    libraryDependencies += scalatest
  )
  .dependsOn(libFun, libJsonParser, libJsonWriter, libJsonQuery)


val libJsonAttributed = project.in(file("json/attributed"))
  .settings(commonSettings)
  .settings(
    name := "json-attributed-query",
    description :=
      """Json object model with support for non-json model attributes.
         | The attributes may define "auxilary" information like source location where element
         | was defined.
         |""".stripMargin,
    libraryDependencies += scalatest
  )
  .dependsOn(libFun, libJsonParser, libJsonWriter, libJsonQuery)


val sampleJsonStreamingFormatter = project.in(file("json/samples/streaming-formatter"))
  .settings(commonSettings)
  .settings(appSettings)
  .settings(
    name := "json-streaming-formatter",
    description :=
      """Sample JSON prettifier/compacter that relies on "asynchronous" (coroutine-based)
        | input-output. This demonstrates how JSON modules could be used to process data without
        | building in-memory tree. It also illustrates "heap-based" recursion (limited by memory and
        | not stack size) and "coroutines". The app relies on synchronous readers/writers but the
        | same approach could be used to integrate with non-blocking NIO input/output facilities.
      """.stripMargin,
    libraryDependencies += scalatest
  )
  .dependsOn(libJsonParser, libJsonWriter)


val libSqlCore = project.in(file("sql/core"))
  .settings(commonSettings)
  .settings(
    name := "sql-core",
    description :=
      """A simple SQL query langaguage DSL. Provides a nice (and safe) SQL query syntax, result
        | set parsing and general connection management. It also provides some common bindings
        | for both input and output arguments.
      """.stripMargin
  )
  .dependsOn(libFun)


val httpHeaders = project.in(file("http/headers"))
  .settings(commonSettings)
  .settings(
    name := "http-headers",
    description :=
      """Common utilities for managing HTTP headers usable for both HTTP clients
        | and servers. The module provides utilities for:
        |  * Dealing with collections of headers
        |  * Converting individual headers to and from domain model.
      """,
    libraryDependencies += scalatest
  )


val httpServerApi = project.in(file("http/server/api"))
  .settings(commonSettings)
  .settings(
    name := "http-server-api",
    description :=
      """Server-related classes, typeclasses and utilites. This module defines a (server-agnostic)
        | general-purpose API that could be leveraged by an HTTP server. For example, this includes:
        | * Routing request based on the request-path.
        | * Accessing request methods and headers
        | * Returning HTTP response. This includes aborting processing early (for example, when
        |   user is not authorized to access the API).
        | * Emiting some side-effects during processing like setting cookies or adding headers.
      """.stripMargin
  )
  .dependsOn(libFun, httpHeaders)


val httpServerToolkit = project.in(file("http/server/toolkit"))
  .settings(commonSettings)
  .settings(
    name := "http-server-toolkit",
    description :=
      """Server toolkit - a set of clasess and utilties that makes writing server API
        | implementations easy. These clasess do not form the client API (i.e. business logic
        | is not expected to use them) but are very handy on the server side as they solve
        | very common problems in the standard way.
      """.stripMargin
  )
  .dependsOn(httpServerApi)


val httpServerJettyGateway = project.in(file("http/server/jetty/gateway"))
  .settings(commonSettings)
  .settings(
    name := "jetty-server-gateway",
    description :=
        """Jetty server main configuration point and very basic handlers.
          | This module is rarely used alone. At least one another module
          | (like jetty-server-qos) should be used for actual business logic
          | implementation.
        """,
    libraryDependencies += "org.eclipse.jetty" % "jetty-server" % jettyVersion,
  )


val httpServerJettyQoS = project.in(file("http/server/jetty/qos"))
  .settings(commonSettings)
  .settings(
    name := "jetty-server-qos",
    description := """Jetty handler with Quality-of-service support.""",
    libraryDependencies ++= Seq(
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      scalatest,
    )
  )
  .dependsOn(libFun, httpServerToolkit, httpServerJettyGateway % Test)


val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "corelib",
    description :=
      """An (opinionated) set of small modular (faceted) libraries for the most common tasks"""
  )
  .aggregate(
    libFun,
    libText,
    libJsonClassic,
    libJsonParser, libJsonWriter, libJsonQuery,
    libJsonSimple, libJsonAttributed,
    sampleJsonStreamingFormatter,

    libSqlCore,

    httpHeaders, httpServerApi, httpServerToolkit,
    httpServerJettyGateway, httpServerJettyQoS,
  )
