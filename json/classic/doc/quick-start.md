# Quick Start

## Build and Install

The library is not hosted in any library repositories so it should 
be built and installed into the repository you use.

The simplest way is to install the library locally:

 1. Check out the library code.
 2. Checkout the tag (version of the library) you want to build.
 3. Run the `sbt libJsonClassic/publishLocal` command inside the project directory
   to publish the libraries into your local repository.

## Add the Library Dependency

Add the library dependency into your build definition:

```sbt
libraryDependencies += "io.github.maxkar" %% "json-classic" % "0.0.1-SNAPSHOT"
```

The actual library version may vary.


## Use the Library

First, you have to import the most important definitions:

```scala
import io.github.maxkar.json.classic.Json
import io.github.maxkar.json.classic.implicits.given

import scala.language.implicitConversions
```

The `Json` object is a main entry point to the json operations as well
as specific JSON types and JSON-Specific exception. The `implicits` 
object provides default conversion between JSON and Scala types. This set
aims to be good enough for the most common use-cases. The last import enables
the automatic conversions by the Scala compiler.


Now you could parse and build json objects:

```scala
val myJson = Json.parse("""{"a" : 3, "arr" : ["test"], "nested" : {"obj" : 3}}""")
val a1 = myJson.a.as[Int] // specify the expected type
val a2: Int = myJson.a // and there is an implicit conversion
val nested = myJson.nested.obj.as[Int] // traverse an object
val v1: Option[Int] = myJson.nonExistent // reading optional values
val v2: Option[Int] = myJson.a // also reading optionals
val items: Seq[String] = json.arr // parsing collections
val jsonMap: Map[String, JsonValue] = myJson.nested // object could be treated as maps

val json1 : JsonValue = Json.make(
  "a" -> a1, // put a value
  "b" -> v1, // optionally put a value
  "c" -> items // put a collection of values convertible to json
)
```
