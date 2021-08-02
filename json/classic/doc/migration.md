# Migration

There are multiple changes compared to Scala 2 version of this library. The goal is
to make _resulting_ API simpler to use. However this involves some migration pain.


## Package Names

All JSON content migrated into the `io.github.maxkar.json.classic` package. The functionality
provided by the library is quite simple but not very configurable. You can't change the way
errors are represented or how to react if a named field is accessed over JSON array. The
platform will eventually include some alternative JSON libraries thus the previous version
is now called `classic` json. As the package rename leads to inevitable changes, the whole
library was moved into a better namespace of `io.github.maxkar`.


## Json Types

There are two changes related to where Json-related types are located. First, the `JsonValue` 
type was renamed into the `Json` type. Second, all specific variations (subtypes and the exception)
was moved inside the `Json` object and lost its prefix. For example, `JsonNumber` now should
be accessed as `Json.Number`.

Now you don't have to import "companion" related entries (Json along with JsonValue in many cases).
The merge of types aims to reduce amount of import clauses. An extra dot for accessing specific
types is not much of a burden and still looks quite clean in the code.


## Json.toString Method

The `toString()` method on istances of the `Json` type now creates a proper JSON representation
instead of dumping internal case-class format.  It is equivalent to the `Json.toString(this)` call.


## Automatic Type Conversions

### Imports 

The conversions from/to json are now represented as the given instances. The Scala 3 syntax
requires explicit notion for importing these instances. Along the lines, the implicit conversions
should be enabled on the usage site(s).

The import of implicit conversions should be replaced with

```scala
import io.github.maxkar.json.classic.implicits.given
import scala.language.implicitConversions
```


### Automatic Conversion Derivation

The library now relies on the automatic derivation of the given instances for `Option`, `Seq`
and `Map` conversions. This greatly reduces amount of boilerplate code (no need to generate
these conveters manually). It also works uniformly for _any_ level of type nesting. For example,
an instance of the `Seq[Seq[Map[String, Seq[Option[Int]]]]` type could be automatically converted 
into JSON by the standad set of conversions. 

No changes are required from developers. However you may be able to remove some unnececssary
code conversions or explicit element conversions to json value.


### New Conversions

New conversions added between `BigDecimal`, `BigInt` and the `Json` type.


### Null Conversions

`null` values are no longer automatically converted into `Json.Null` in collections. Please
use `Json.Null` explicitly or use `Option` to represent the optional value in your code.


## Encoding Auto-detection

The RFC 8259 (lastest JSON specification at the moment of the library creation) sets the `UTF-8`
charset as a standard for the JSON encoding. The library agrees with this definition and
assumes all input to `parse(Array[Byte])` has `UTF-8` encoding. The automatic detection is
now provided by the `parseRFC7158(Array[Byte])` method. Developers are encouraged to use
`parse` method where possible and revert to fallback of `parseRFC7158` only when they know
they may get non-UTF-8 input.
