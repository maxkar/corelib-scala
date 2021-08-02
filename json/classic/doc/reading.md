# Reading JSON

## Parsing Input

The input is parsed by using one of the `Json.parse` (UTF-8 for `Array[Byte]` overload)
or `Json.parseRFC7158` (automatic encoding detection specified by previous RFCs) methods. 
The method supports both complex (arrays and objects) and simple (numbers and strings) values.


## Accessing Members

The memeber accessors (field and index) are defined on the `Json` type.
Applying accessor to the `Json.Undefined` value is always valid and 
will always produce the `Json.Undefined` value.
Attempt to use an accessor on a primitive value or on a value not supporting
the accessor will also return `Json.Undefined`


### Field Accessors

There are two ways to access a field of an object:

```scala
val x : Json = ???
val value1 = x.member // field-based syntax
val value2 = x("member") // apply-based syntax
```

The field-based syntax should be used when possible. The apply-based
syntax may be used if the field name is generated dynamically,
if the field name is not valid Scala identifier or if the named access
is followed by an indexed access.

The resulting value is as follows:

 * If x is `Json.Object` and the field `member` is defined - the falue of the field
 * Otherwise the `Json.Undefined`


### Indexed Accessor

Retrieving the value by index is done in the following way:

```scala
val x : Json = ???
val v = x(5)
```

The resulting value is as follows:

 * If x is `Json.Array` and index is inside array bounds (0 to length - 1) then the value is returned
 * Otherwise the `Json.Undefined`

Negative index values have no special meaning. Passing a negative index will always result
in the `Json.Undefined` value.


## Converting Values

The value could be cast to a required type by either exlicitly using the `as` method
or by using `Json` value in a context where some other type is expected. The standard
set of conversions is consistent so if there is an implpicit conversion from
`Json` to `T` then you could also use `as` conversion and vice versa.

All the following are valid conversions:

```scala
def test(v : Int) = ???
val x : Json = ???

val a = x.as[Int]
val b : Int = x
test(x)
```

The default "Optional" converters works as follows:
 * If the value is `Json.Undefined` or `Json.Null` - None
 * Value is defined and is compatible to the expected type (Number for Ints, Strings for strings) - Some(value)
 * Otherwise a `Json.Exception` is raised.

In particular, it means that an attempt to convert `Json.Number` to `Option[String]` will cause an exception
(there is no automatic conversion from numbers to string), not the None value.
