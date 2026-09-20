package io.github.maxkar
package fun.coroutine

import fun.typeclass.Monad

/**
 * Implementation of the Input/Output coroutine.
 * Illustrates potential IO as coroutine (where IO may be asynchronous).
 */
final class IOCoroutine extends org.scalatest.funsuite.AnyFunSuite {
  import IOCoroutine._


  /**
   * Reads the data, writes "next char" for every char read and returns
   * number of characters processed. Does that using quite ugly recursion.
   */
  def incrIO: Routine[Int] =
    read flatMap {
      case None => Monad.pure(0)
      case Some(c) =>
        for
          _ <- write((c + 1).toChar)
          cnt <- incrIO
        yield cnt + 1
    }


  test("Smoke test") {
    assert(("", 0) === runIO("", incrIO))
    assert(("bcd", 3) === runIO("abc", incrIO))
    assert(("XYZ236", 6) === runIO("WXY125", incrIO))
  }


  test("Huuuuge strings") {
    val hugeNum = 1000000
    val hugeI = "a" + ("x" * hugeNum) + "b"
    val hugeO = "b" + ("y" * hugeNum) + "c"

    assert((hugeO, hugeNum + 2) === runIO(hugeI, incrIO))
  }
}


object IOCoroutine {
  /** State suspension. Hard-coded state type. */
  enum Call[T] {
    case Read extends Call[Option[Char]]
    case Write(value: Char) extends Call[Unit]
  }

  type Routine[T] = Coroutine[Call, T]

  /** Reads next value from the "input stream". */
  val read: Routine[Option[Char]] = Coroutine.call(Call.Read)


  /** Writes value into the "output stream". */
  def write(c: Char): Routine[Unit] = Coroutine.call(Call.Write(c))


  /** Runs the IO routine. */
  def runIO[T](input: String, routine: Routine[T]): (String, T) =
    var ptr = 0
    var output = new StringBuilder()
    var proc = routine.run()

    /* Classical const-stack runner. Real IO (with async/nio streams) would probably
     * be const-stack as well but with results achieved by different means.
     */
    while true do {
      proc match {
        case Flow.Done(x) =>
          return (output.toString(), x)
        case Flow.Call(Call.Read, c) =>
          val iores =
            if ptr < input.length() then
              val r = input.charAt(ptr)
              ptr += 1
              Some(r)
            else
              None
            end if
          proc = c(iores)
        case Flow.Call(Call.Write(v), c) =>
          output += v
          proc = c(())
      }
    }
    throw new Error("Please stop reaching unreacheable code")
  end runIO
}
