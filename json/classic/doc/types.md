# JSON Types

The library uses an extension of the standard JSON types. All the JSON
types are children of the base `Json` type. The children (leaf) types
are:

 * `Json.Null` - the null literal
 * `Json.True` and `Json.False` - boolean constants
 * `Json.String` - string literal
 * `Json.Number` - numeric literal. The library stores each number as a string
   encoding of the value to prevent any data loss (for example, some users may 
   want to know a number of trailing zeros in a floating-point number).
 * `Json.Array` - array of json values
 * `Json.Object` - json object, a map from strings to JsonValues
 * `Json.Undefined` - a special "undefined" object to support nice 
   access and serialization APIs. This value is not part of the core
   JSON specification but provides support for "optional" values. 
   It is returned on an access to non-existent property. The standar
   conversions to `Option` types convert the `Json.Undefined` into
   the `None` value. `Json.Undefined` values are ignored when a 
   new Array or Object is created (however, `Json.Null`s are explicitly 
   serialized as `null` literals)
