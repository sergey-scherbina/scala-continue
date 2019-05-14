package monad.cont

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.Paths

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

  test("monad reflection") {
    val v: List[Int] = reify(for {
      x <- reflect(List(1, 2, 3))
      y <- reflect(List(1, 2, 3))
    } yield x + y)

    println(v)
  }

  test("file") {

    type IOResult[A] = Throwable Either A
    type Chunk[A] = (Long, IOResult[(Int, A)])

    def completionHandler[A](f: A => IOResult[Int] => Unit) =
      new CompletionHandler[Integer, A] {
        override def completed(r: Integer, a: A): Unit = f(a)(Right(r))

        override def failed(exc: Throwable, a: A): Unit = f(a)(Left(exc))
      }

    def read(ch: AsynchronousFileChannel, p: Long, b: ByteBuffer) =
      shift((f: Chunk[ByteBuffer] => Unit) => ch.read(b, p, b,
        completionHandler[ByteBuffer](b => r => f((p, r.map((_, b)))))))

    def readAll(ch: AsynchronousFileChannel, p: Long, z: Int): Cont[Unit, Unit, Stream[Chunk[ByteBuffer]]] = {
      println(p)
      if (p >= ch.size()) pure(Stream.empty) else
        for {x <- read(ch, p, ByteBuffer.allocate(z))
             y <- readAll(ch, p + z, z)
        } yield x #:: y
    }

    val chan = AsynchronousFileChannel
      .open(Paths.get("src/test/resources/hello.txt"))

    reset(for (x <- readAll(chan, 0, 1)) yield {
      x.foreach(println)
    })

    println("exit")

  }

}
