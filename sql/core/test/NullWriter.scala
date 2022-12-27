package io.github.maxkar
package sql

import java.io.Writer


/** No-op writer. */
object NullWriter extends Writer:
  override def close(): Unit = ()
  override def flush(): Unit = ()
  override def write(x: Array[Char], y: Int, z: Int): Unit = ()
  override def append(x: CharSequence): Writer = this
end NullWriter
