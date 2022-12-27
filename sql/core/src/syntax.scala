package io.github.maxkar
package sql

/** Facade for the most common syntax and imports. */
object syntax:
  export query.syntax.*
  export query.syntax.given
  export query.Batch
  export result.syntax.*
  export result.Row
end syntax
