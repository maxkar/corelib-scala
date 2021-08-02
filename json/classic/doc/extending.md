# Extending Conversions

The default set of conversions is provided by the library. However,
clients may want to provide their own conversions. It is usually advised
to explicitly call conversion methods for user types. However,
the library does not enforce that and for some "basic" types
(like dates) an automatic conversion could be desirable.

This type of extension is done by providing implicit values and
implicit conversions.

## Scala to Json

To define a conversion from Scala to `Json` you have to define a _given_
instance of `scala.Conversion` for your type and Json.

```scala
given dateToJson: Conversion[Date, Json] = 
  new Conversion[Date, Json] {
    override def apply(v: Date): Json = v.toString()
  }
```


## Json to Scala

To define a conversion from `Json` to a Scala type you have to define an 
appropriate instance of the `scala.Conversion`:

```scala
given jsonToDate: Conversion[Json, Date] =
  new Conversion[Json, Date] {
    override def apply(v: Json): Date = new Date(v.as[String])
  }
```


## Collection and Optional Conversions

The library automatically derive conversions for optional values, sequence and maps. These
conversions may be generated recursively (i.e. Sequences of sequences, sequenses of maps to optional
values and so on). Such conversions are automatically derived from _two_ other implicit conversions:
 
 * A conversion from/to `Json` to/from type `T`
 * A conversion from/to `Json` to/from a "collection" of Json values `C[Json]`

Changes in the second conversion (for example, different implementation of the `Json` to `Option[Json]`)
conversion will affect all other automatically derived types (for example, it will affect conversion
from `Json` to `Option[Int]`).
