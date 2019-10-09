package cont

import java.io.{BufferedReader, StringReader}

import org.scalatest.FunSuite

import scala.util.Try

class PipesTest extends FunSuite {

  trait Read[A] {
    def read(s: String): A

    def readLine(r: BufferedReader): A = read(r.readLine())
  }

  object Read {
    def apply[A: Read]: Read[A] = implicitly[Read[A]]

    implicit object readInt extends Read[Int] {
      override def read(s: String): Int = s.toInt
    }

  }

  test("pipes") {
    import cont.Cont._

    type Pipe[I, O, A] = A :#: (InCont[I] => OutCont[O] => Unit)
    trait InCont[I] extends (OutCont[I] => Unit)
    trait OutCont[O] extends (O => InCont[O] => Unit)

    def InCont[I](k: OutCont[I] => Unit): InCont[I] = k(_)

    def OutCont[O](k: O => InCont[O] => Unit): OutCont[O] = k(_)

    def input[I, O]: Pipe[I, O, I] = k => ki => ko =>
      ki(OutCont(i => ki1 => k(i)(ki1)(ko)))

    def output[I, O](o: O): Pipe[I, O, Unit] = k => ki => ko =>
      ko(o)(InCont(ko1 => k()(ki)(ko1)))

    def merge[I, O, M, A](p: Pipe[I, M, A], q: Pipe[M, O, A]): Pipe[I, O, A] =
      k => ki => ko => q(_ => ???)(InCont(ko1 => p(_ => ???)(ki)(ko1)))(ko)

    def runPipeIO[I: Read, O](p: Pipe[I, O, Unit])(r: BufferedReader) = {
      lazy val ki: InCont[I] = InCont(_ (Read[I].readLine(r))(ki))
      lazy val ko: OutCont[O] = OutCont { o => k => println(o); k(ko) }
      p(_ => _ => _ => ())(ki)(ko)
    }

    //////

    def double(): Pipe[Int, Int, Unit] = for {
      i <- input[Int, Int]
      _ <- output[Int, Int](i * 2)
      _ <- double()
    } yield ()

    def quad: Pipe[Int, Int, Unit] = merge(double(), double())

    println(Try(runPipeIO(quad)(
      new BufferedReader(new StringReader(
        "1\n" * 100
      )))))
  }

  test("pipes2") {
    type Cont[A, S, R] = (A => S) => R
    type :#:[A, R] = Cont[A, R, R]

    @inline def pure[A, R](a: A): A :#: R = _ (a)

    @inline def bind[A, S1, R, B, S2](c: (A => S1) => R)(f: A => (B => S2) => S1): (B => S2) => R =
      k => c(f(_)(k))

    @inline def fmap[A, S, R, B](c: (A => S) => R)(f: A => B): (B => S) => R =
      k => c(a => k(f(a)))

    implicit class ContMonad[A, S, R](val c: (A => S) => R) {
      @inline def flatMap[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = bind(c)(f)

      @inline def map[B](f: A => B): Cont[B, S, R] = fmap(c)(f)
    }

    type Pipe[I, O, A] = A :#: (InCont[I] => OutCont[O] => Unit)
    trait InCont[I] extends (OutCont[I] => Unit)
    trait OutCont[O] extends (O => InCont[O] => Unit)

    def InCont[I](k: OutCont[I] => Unit): InCont[I] = k(_)

    def OutCont[O](k: O => InCont[O] => Unit): OutCont[O] = k(_)

    def input[I, O]: Pipe[I, O, I] = k => ki => ko =>
      ki(OutCont(i => ki1 => k(i)(ki1)(ko)))

    def output[I, O](o: O): Pipe[I, O, Unit] = k => ki => ko =>
      ko(o)(InCont(ko1 => k()(ki)(ko1)))

    def merge[I, O, M, A](p: Pipe[I, M, A], q: Pipe[M, O, A]): Pipe[I, O, A] =
      k => ki => ko => q(_ => ???)(InCont(ko1 => p(_ => ???)(ki)(ko1)))(ko)

    def runPipeIO[I: Read, O](p: Pipe[I, O, Unit])(r: BufferedReader) = {
      lazy val ki: InCont[I] = InCont(_ (Read[I].readLine(r))(ki))
      lazy val ko: OutCont[O] = OutCont { o => k => println(o); k(ko) }
      p(_ => _ => _ => ())(ki)(ko)
    }

    //////

    def double(): Pipe[Int, Int, Unit] = for {
      i <- input[Int, Int]
      _ <- output[Int, Int](i * 2)
      _ <- double()
    } yield ()

    def quad: Pipe[Int, Int, Unit] = merge(double(), double())

    println(Try(runPipeIO(quad)(
      new BufferedReader(new StringReader(
        "1\n" * 100
      )))))
  }
}
