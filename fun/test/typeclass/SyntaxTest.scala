package io.github.maxkar
package fun.typeclass

/** Some tests for the monad/function syntax. */
final class SyntaxTest extends org.scalatest.funsuite.AnyFunSuite:

  test("Minimal monad implementation does not have cycles") {
    given Monad[Option] with
      def pure[T](v: T): Option[T] = Some(v)
      def bind[S, R](v: Option[S], fn: S => Option[R]): Option[R] = v.flatMap(fn)

    def calc[M[_]: Monad](x: M[Int]): M[Int] =
      for
        a <- x
        b <- Monad.pure(3)
        c <- x
      yield
        (a + b) * c


    val a: Option[Int] = Some(2)
    val b: Option[Int] = Some(5)

    assert(calc(a) === Some(10))
    assert(calc(b) === Some(40))
  }



  test("Syntax extensions works") {
    given Monad[Option] with
      def pure[T](v: T): Option[T] = Some(v)
      def bind[S, R](v: Option[S], fn: S => Option[R]): Option[R] = v.flatMap(fn)

    val a: Option[Int] = Some(2)
    val b: Option[Int] = Some(5)

    def calc[M[_]: Monad](x: Int)(y: Int): M[Int] =
      Monad.pure(x + y)

    assert((calc ||> a |||> b) === Some(7))
  }


  test("Deconstruction works without extra syntax on monad") {
    given Monad[Option] with
      def pure[T](v: T): Option[T] = Some(v)
      def bind[S, R](v: Option[S], fn: S => Option[R]): Option[R] = v.flatMap(fn)

    def sumP[M[_]: Monad](x: M[(Int, Int)]): M[Int] =
      for
        (a, b) <- x
      yield
        a + b

    val a: Option[(Int, Int)] = Some((2, 3))

    assert(sumP(a) === Some(5))
  }

end SyntaxTest
