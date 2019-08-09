package monad.cont

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.charset.Charset
import java.nio.file.Paths

import org.scalatest.FunSuite

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.Try

class ContTest extends FunSuite {

  test("Fib") {

    def fib() = loop(for {
      (x, y) <- take[(Int, Int), Int << Unit]
      _ <- suspend[Unit, Int](x)
    } yield (y, x + y))(1, 1)

    println(fib().forAll().take(10).toList)
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
    val div = reset0 {
      for {
        x <- take[Int, Int => IntOrStr]
        _ = println("x=" + x)
        y <- take[Int, IntOrStr]
        _ = println("y=" + y)
        _ <- shift0((k: Int => IntOrStr) =>
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
    val f1 = reset0 {
      for {
        x <- take[Int, String => IntOrStr]
        _ = println("x=" + x)
        y <- take[String, IntOrStr]
        _ <- shift0((k: String => IntOrStr) =>
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
      _ <- shift0((k: Int => Int) =>
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
    val f4: String => String << Int = loop(for {
      x <- channel[Int, String]
      _ = println("x:" + x)
    } yield x.toString)

    println(f4("Hello"))
  }

  test("suspend") {
    def f4 = loop(for {
      x <- take[Int, String << Int]
      _ = println("x = " + x)
      y <- suspend[Int, String](s"[$x]")
      _ = println("y = " + y)
    } yield x + y)

    println()
    println(f4(1)(2)(3)(4))
  }

  test("monad reflection") {
    val v: List[(String, Int)] = reify(for {
      x <- reflect[List, String, (String, Int)](List("1", "2", "3"))
      y <- reflect[List, Int, (String, Int)](List(1, 2, 3))
    } yield (x, y))

    println(v)

    val v2: List[(String, Int)] = reify(for {
      x <- List("1", "2", "3").reflect[(String, Int)]
      y <- List(1, 2, 3).reflect[(String, Int)]
    } yield (x, y))

    println(v2)

  }

  test("file") {

    type IOResult[A] = Throwable Either A
    type Chunk[A] = (Long, IOResult[(Int, A)])

    def handler[A](f: A => IOResult[Int] => Unit) =
      new CompletionHandler[Integer, A] {
        override def completed(r: Integer, a: A): Unit = f(a)(Right(r))

        override def failed(exc: Throwable, a: A): Unit = f(a)(Left(exc))
      }

    def read(ch: AsynchronousFileChannel, p: Long, b: ByteBuffer) =
      shift0((f: Chunk[ByteBuffer] => Unit) => ch.read(b, p, b,
        handler[ByteBuffer](b => r => f((p, r.map((_,
          b.flip().asInstanceOf[ByteBuffer])))))))

    def readStream(ch: AsynchronousFileChannel, p: Long,
                   z: Int): Cont[Unit, Unit, Stream[Chunk[ByteBuffer]]] =
      if (p >= ch.size()) pure(Stream.empty) else
        for {x <- read(ch, p, ByteBuffer.allocate(z))
             y <- readStream(ch, p + z, z)
             } yield x #:: y

    def decode(x: Chunk[ByteBuffer]): Chunk[String] = (x._1, x._2.map(y =>
      (y._1, Charset.defaultCharset().decode(y._2).toString)))

    val chan = AsynchronousFileChannel
      .open(Paths.get("src/test/resources/hello.txt"))

    reset0 {
      for (x <- readStream(chan, 0, 7)) yield {
        println("He")
        x.map(decode).foreach(println)
      }
    }

    println("exit")

  }

  test("futures") {

    def decode(x: ByteBuffer): String = Charset.defaultCharset()
      .decode(x.flip().asInstanceOf[ByteBuffer]).toString

    def handler[A](f: Throwable Either A => Unit) =
      new CompletionHandler[Integer, A] {
        override def failed(exc: Throwable, a: A): Unit = f(Left(exc))

        override def completed(r: Integer, a: A): Unit = if (r < 0)
          f(Left(new IOException("EOF"))) else f(Right(a))
      }

    def readChunk[A](ch: AsynchronousFileChannel, p: Long, b: ByteBuffer) = {
      val pr = Promise[(Long, ByteBuffer)]
      ch.read(b, p, (p, b), handler[(Long, ByteBuffer)](x => pr.complete(x.toTry)))
      pr.future
    }

    def readFile(ch: AsynchronousFileChannel, p: Long, n: Int)(
      implicit executor: ExecutionContext): Future[Stream[ByteBuffer]] =
      if (p >= ch.size()) Future.successful(Stream.empty) else for {
        (x, y) <- readChunk(ch, p, ByteBuffer.allocate(n))
        z <- readFile(ch, x + y.limit(), n)
      } yield y #:: z

    val chan = AsynchronousFileChannel
      .open(Paths.get("src/test/resources/hello.txt"))

    import concurrent.ExecutionContext.Implicits._

    val z = for (s <- readFile(chan, 0, 1))
      yield for (b <- s) println(decode(b))

    Await.result(z, Duration.Inf)

    println("exit")

  }

  test("asyncIO") {

    def handler[A](f: Try[A] => Unit) =
      new CompletionHandler[Integer, A] {
        override def failed(exc: Throwable, a: A): Unit =
          f(Left(exc).toTry)

        override def completed(r: Integer, a: A): Unit =
          if (r > 0) f(Right(a).toTry)
          else f(Left(new IOException("EOF")).toTry)
      }

    type IOChunk = Try[(ByteBuffer, Long)]

    def read(channel: AsynchronousFileChannel) =
      shift0((k: IOChunk => IOChunk) => loop(for {
        chunk <- take[IOChunk, Unit]
        r <- shift0((f: IOChunk => Unit) =>
          chunk.fold[Unit](_ => stop(), c =>
            channel.read(c._1, c._2, c, handler(f))))
      } yield k(r)))

    def readFile(channel: AsynchronousFileChannel) =
      shift0((k: IOChunk => IOChunk) => reset0(
        for (x <- read(channel)) yield k(x).map(c => (
          c._1.flip().asInstanceOf[ByteBuffer], c._2 + c._1.limit()
        ))))

    def decode(x: ByteBuffer): String = Charset.defaultCharset()
      .decode(x.flip().asInstanceOf[ByteBuffer]).toString

    val chan = AsynchronousFileChannel
      .open(Paths.get("src/test/resources/hello.txt"))

    reset0(for {
      x <- readFile(chan)
      _ = for (y <- x)
        println(decode(y._1))
    } yield x)(
      Right(ByteBuffer.allocate(1),
        0L).toTry
    )

    println("exit")

  }

  test("Alice has a cat.") {

    def alice() =
      shift0((k1: String => String) =>
        shift0((k2: String => String) =>
          "Alice" + k1(k2("."))))

    val cat = reset0(for (x <- reset0(for (y <- alice())
      yield " has " + y)) yield "a cat" + x)

    println(cat)

  }

}
