package io.github.maxkar
package json.sample.formatter.streaming

import fun.instances.Unnest
import json.writer.Layout

import java.io.IOException

/** Main "pipe" application. */
object App {
  def main(args: Array[String]): Unit = {
    if args.length <= 0 then {
      System.err.println("No arguments provided, you should specify either 'compact' or 'prettify' as an option")
      System.exit(4)
    }

    val layout: Layout[Unnest, FormatterIO] =
      if args(0) == "compact" then
        Layout.Compact()
      else if args(0) == "prettify" then
        Layout.Indent(4)
      else {
        System.err.println("Illegal mode provided, you should specify either 'compact' or 'prettify' as an option")
        System.exit(5)
        throw new Error("Keep reaching unreacheable code")
      }

    val ow = new java.io.BufferedWriter(new java.io.OutputStreamWriter(System.out, "UTF-8"))
    val ir = new java.io.InputStreamReader(System.in, "UTF-8")
    val io = new FormatterIO(ir, ow)

    try {
      StreamingFormatter.formatFully(io, layout)
    } catch {
      case e: IOException =>
        System.err.println(e.getMessage())
        System.exit(6)
    }
  }
}
