package monad.cont

import org.scalatest.FunSuite

class ContTest extends FunSuite {

  test("Fib") {
    def fib() = loop(for {
      (x, y) <- take[(Int, Int), Int @@ Unit]
      _ <- suspend[Unit, Int](x)
    } yield (y, x + y))(1, 1) putAll()

    println(fib().take(10).toList)
    println()
  }

  test("Fib2") {
    def fib() = loop(for {
      (x, y) <- take[(BigInt, BigInt), Stream[BigInt]]
      _ <- put(x)} yield (y, x + y))(1, 1)

    println(fib().take(100).toList)
    println()
  }

  type IntOrStr = String Either Int

  test("Div") {
    val div = reset {
      for {
        x <- take[Int, Int => IntOrStr]
        _ = println("x=" + x)
        y <- take[Int, IntOrStr]
        _ = println("y=" + y)
        _ <- shift((k: Int => IntOrStr) =>
          if (y == 0) Left("Error: divide by zero")
          else k(y))
      } yield {
        println("x / y = " + (x / y))
        Right(x / y)
      }
    }

    val d10 = div(10)
    println("----")
    println(d10(2))
    println("----")
    println(d10(3))
    println("----")
    println(d10(0))
    println("----")
  }

  test("take") {
    val f1 = reset {
      for {
        x <- take[Int, String => IntOrStr]
        _ = println("x=" + x)
        y <- take[String, IntOrStr]
        _ <- shift((k: String => IntOrStr) =>
          if (y == "") Right(x) else k(y))
        _ = println("y=" + y)
      } yield Left(y + " " + x)
    }

    println(f1(10)("Hello"))
    println()
    println(f1(10)(""))
  }

  test("loop") {
    val f2 = loop(for {
      x <- take[Int, Int]
      _ <- shift((k: Int => Int) =>
        if (x <= 10) k(x) else x)
      _ = println(x)
    } yield x + 1)

    println()
    println("Loop:")
    println(f2(1))
  }

  test("nat") {
    def nat(n: Int) = loop(for {
      x <- take[Int, Stream[Int]]
      _ <- put(x)
    } yield x + 1)(n)

    println(nat(10).take(10).toList)
  }

  test("nat2") {
    def nat(n: Int) = unfold(n)(_ + 1)

    println(nat(10).take(10).toList)
  }

  test("nat3") {
    def nat(n: Int) = loop(for {
      x <- channel[Int, Int]
      _ = println("x:" + x)
    } yield x + 1)(n) toStream

    println(nat(10).take(10).toList)
  }

  test("channel") {
    val f4: String => String @@ Int = loop(for {
      x <- channel[Int, String]
      _ = println("x:" + x)
    } yield x.toString)

    println(f4("Hello"))
  }

  test("suspend") {
    def f4 = loop(for {
      x <- take[Int, String @@ Int]
      _ = println("x = " + x)
      y <- suspend[Int, String](s"[$x]")
      _ = println("y = " + y)
    } yield x + y)

    println()
    println(f4(1)(2)(3)(4))
  }

}
