package cont

import java.io.{BufferedReader, StringReader}

import scala.util.Try
import scala.util.control.TailCalls._

object Pipes2 extends App {

  type ![A] = TailRec[A]
  type =>>[A, B] = A => ![B]

  final case class Cont[A, S, R](cont: ![(A =>> S) =>> R]) extends AnyVal {
    @inline def flatMap[B, S1](f: A => Cont[B, S1, S]): Cont[B, S1, R] =
      Cont(done(k => cont.flatMap(_ (f(_).cont.flatMap(_ (k))))))

    @inline def map[B](f: A => B): Cont[B, S, R] =
      Cont(done(k => cont.flatMap(_ (a => k(f(a))))))

    @inline def >>=[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = flatMap(f)

    @inline def >>[B, C](c2: Cont[B, C, S]): Cont[B, C, R] = flatMap(_ => c2)

    @inline def >>>(s: S): Cont[S, S, R] = map(_ => s)
  }

  type :#:[A, R] = Cont[A, R, R]

  @inline final def pure[A, R](a: A): A :#: R = Cont(done(_ (a)))

  @inline final def shift0[A, S, R](k: (A =>> S) =>> R): Cont[A, S, R] = Cont(done(k))

  @inline final def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = shift0(k => done(f(k(_).result)))

  @inline final def reset[A, R](k: Cont[A, A, R]): R = k.cont.result(done).result

  type ~>[A, B] = A => B => ![Unit]

  trait InCont[I] extends (Unit ~> OutCont[I])

  def InCont[I](k: (Unit ~> OutCont[I])): InCont[I] = k(_)

  trait OutCont[O] extends (O ~> InCont[O])

  def OutCont[O](k: O ~> InCont[O]): OutCont[O] = k(_)

  type Pipe[I, O, A] = A :#: (InCont[I] ~> OutCont[O])

  def input[I, O]: Pipe[I, O, I] = shift(k => ki => ko =>
    ki()(OutCont(i => k1 => tailcall(k(i)(k1)(ko)))))

  def output[I, O](o: O): Pipe[I, O, Unit] = shift(k => ki => ko =>
    ko(o)(InCont(_ => k1 => tailcall(k()(ki)(k1)))))

  def merge[I, O, M, A](p: Pipe[I, M, A], q: Pipe[M, O, A]): Pipe[I, O, A] =
    shift(k => ki => ko => q.cont.flatMap(_ (_ => ???)).flatMap(_ (
      InCont(_ => k1 => p.cont.flatMap(_ (_ => ???)).flatMap(_ (ki)(k1))))(ko)))

  def runPipeIO[I: Read, O](p: Pipe[I, O, Unit])(r: BufferedReader): Unit = {
    lazy val ki: InCont[I] = InCont(_ => k => (k(Read[I].readLine(r))(ki)))
    lazy val ko: OutCont[O] = OutCont { o => k => println(o); k()(ko) }
    p.cont.flatMap(_ (_ => done(_ => _ => done())).flatMap(_ (ki)(ko))).result
  }

  //////

  def double(): Pipe[Int, Int, Unit] = for {
    i <- input[Int, Int]
    _ <- output[Int, Int](i * 2)
    _ <- double()
  } yield ()

  def quad(): Pipe[Int, Int, Unit] = merge(double(), double())

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

  Try(
    runPipeIO(quad())(new BufferedReader(new StringReader("1\n" * 10000)))
  )

}
