package io.github.maxkar
package json.sample.formatter.streaming

/** Main "pipe" application. */
object App {
  def main(args: Array[String]): Unit =
    if args.length <= 0 then
      System.err.println("No arguments provided, you should specify either 'compact' or 'prettify' as an option")
      System.exit(4)

    val mode =
      if args(0) == "compact" then
        Formatter.compact
      else if args(0) == "prettify" then
        Formatter.prettify
      else
        System.err.println("Illegal mode provided, you should specify either 'compact' or 'prettify' as an option")
        System.exit(5)
        throw new Error("Keep reaching unreacheable code")

    val ow = new java.io.OutputStreamWriter(System.out, "UTF-8")
    val res = mode( new java.io.InputStreamReader(System.in, "UTF-8"), ow)
    ow.close()
    res match
      case None =>
        System.out.println()
      case Some(err) =>
        System.err.println(err)
        System.exit(6)
    end match
  end main
}
