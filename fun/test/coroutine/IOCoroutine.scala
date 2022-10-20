package io.github.maxkar
package fun.coroutine

import fun.typeclass.Monad


/**
 * Implementation of the Input/Output coroutine.
 * Illustrates potential IO as coroutine (where IO may be asynchronous).
 */
final class IOCoroutine extends org.scalatest.funsuite.AnyFunSuite:
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
end IOCoroutine


object IOCoroutine:
  /** Coroutine module. */
  val module = new Coroutine[IOSus]
  import module._

  export module.given
  export module.Routine


  /** State suspension. Hard-coded state type. */
  enum IOSus[T]:
    case Read(cont: Option[Char] => module.RunResult[T])
    case Write(value: Char, cont: Unit => module.RunResult[T])
  end IOSus


  /** Reads next value from the "input stream". */
  val read: Routine[Option[Char]] =
    module.suspend(new Suspender[Option[Char]] {
      override def encode[V](continue: Option[Char] => RunResult[V]): IOSus[V] =
        IOSus.Read(continue)
    })


  /** Writes value into the "output stream". */
  def write(c: Char): Routine[Unit] =
    module.suspend(new Suspender[Unit] {
      override def encode[V](continue: Unit => RunResult[V]): IOSus[V] =
        IOSus.Write(c, continue)
    })


  /** Runs the IO routine. */
  def runIO[T](input: String, routine: Routine[T]): (String, T) =
    var ptr = 0
    var output = new StringBuilder()
    var cont: Unit => RunResult[T] = _ => module.run(routine)

    /* Classical const-stack runner. Real IO (with async/nio streams) would probably
     * be const-stack as well but with results achieved by different means.
     */
    while true do
      cont(()) match
        case Coroutine.RunResult.Finished(x) => return (output.toString(), x)
        case Coroutine.RunResult.Suspended(IOSus.Read(c)) =>
          val iores =
            if ptr < input.length() then
              val r = input.charAt(ptr)
              ptr += 1
              Some(r)
            else
              None
            end if
          cont = _ => c(iores)
        case Coroutine.RunResult.Suspended(IOSus.Write(v, c)) =>
          output += v
          cont = c
      end match
    end while
    throw new Error("Please stop reaching unreacheable code")
  end runIO
end IOCoroutine
