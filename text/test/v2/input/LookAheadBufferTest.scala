package io.github.maxkar
package text.v2.input

import text.Location

final class LookAheadBufferTest extends org.scalatest.funsuite.AnyFunSuite {
  bufferTest("simple operations with no wrap-around", 10) { buffer =>
    val writeBuffer = buffer.writeBuffer

    for {
      i <- 0 until writeBuffer.length
    } {
      writeBuffer(i) = ('0' + i).toChar
    }

    assert(buffer.writeStart === 0)
    assert(buffer.writeEnd === 10)

    buffer.dataWritten(3)
    assert(buffer.writeStart === 3)
    assert(buffer.writeEnd === 10)
    assert(buffer.size === 3)
    assert(buffer.lookAhead(2) == '2')

    buffer.dataWritten(4)
    assert(buffer.writeStart === 7)
    assert(buffer.writeEnd === 10)
    assert(buffer.size === 7)
    assert(buffer.lookAhead(2) == '2')

    assert(buffer.location === Location(0, 1, 1))

    buffer.skip(1)
    assert(buffer.writeStart === 7)
    assert(buffer.writeEnd === 10)
    assert(buffer.size === 6)
    assert(buffer.lookAhead(2) == '3')
    assert(buffer.location === Location(1, 1, 2))

    val dst = new Array[Char](4)
    assert(buffer.read(dst, 1, 3) === 2)
    assert(dst === Array(0, '1', '2', 0))
    assert(buffer.writeStart === 7)
    assert(buffer.writeEnd === 10)
    assert(buffer.size === 4)
    assert(buffer.lookAhead(2) == '5')
    assert(buffer.location === Location(3, 1, 4))
  }


  bufferTest("operations on a wrap-around buffer", 10) { buffer =>
    val writeBuffer = buffer.writeBuffer

    for {
      i <- 0 until writeBuffer.length
    } {
      writeBuffer(i) = ('0' + i).toChar
    }

    assert(buffer.writeStart === 0)
    assert(buffer.writeEnd === 10)

    buffer.dataWritten(10)
    assert(buffer.writeStart === 0)
    assert(buffer.writeEnd === 0)
    assert(buffer.size === 10)
    assert(buffer.lookAhead(2) == '2')

    buffer.skip(5)
    assert(buffer.writeStart === 0)
    assert(buffer.writeEnd === 5)
    assert(buffer.size === 5)
    assert(buffer.lookAhead(2) == '7')

    writeBuffer(0) = 'A'
    writeBuffer(1) = 'B'
    writeBuffer(2) = 'C'
    buffer.dataWritten(3)
    assert(buffer.writeStart === 3)
    assert(buffer.writeEnd === 5)
    assert(buffer.size === 8)
    assert(buffer.lookAhead(2) == '7')

    val dst = new Array[Char](6)
    assert(buffer.read(dst, 0, 6) === 6)
    assert(dst === Array('5', '6', '7', '8', '9', 'A'))
    assert(buffer.writeStart === 3)
    assert(buffer.writeEnd === 10)
    assert(buffer.size === 2)
    assert(buffer.lookAhead(1) == 'C')
    assert(buffer.location === Location(11, 1, 12))
  }


  private def bufferTest(name: String, capacity: Int = 10)(callback: LookAheadBuffer => Unit) =
    test(name) {
      callback(LookAheadBuffer(capacity))
    }
}


