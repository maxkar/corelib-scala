package io.github.maxkar
package sql.query

object syntax:
  /** SQL Query interpolation. */
  extension (ctx: StringContext)
    def sql(args: Fragment*): Query =
      new Query(ctx.parts, args)
  end extension
end syntax
