# Library Principles

 * No magic. Reading and writing objects is code (not type and annotations).
 * No global configuration. Everything is local to a compilation unit or lexical scope.
   Moreover, the default conversions may be changed if needed.
 * Extensible. Developers should be able to add conversions for their types.
 * Immutable. No way to change an existing value.
 * Typesafe. No conversion between "related" types. Json number may be converted to
   int but not to String.
 * Convenient interface. Writing optional fields should be easy. Parsing objects should
   not be too hard.
