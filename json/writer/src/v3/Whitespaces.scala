package io.github.maxkar
package json.writer.v3

private [v3] class Whitespaces(count: Int) extends CharSequence {
  override def charAt(index: Int): Char = ' '
  override def length(): Int = count
  override def subSequence(start: Int, end: Int): CharSequence = new Whitespaces(end - start)
}

