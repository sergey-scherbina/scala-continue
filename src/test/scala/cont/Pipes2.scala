package cont

import java.io.{BufferedReader, StringReader}

import scala.util.Try
import scala.util.control.TailCalls._

object Pipes2 extends App {

  type ![A] = TailRec[A]
  type =>>[A, B] = A => ![B]
  type :#:[A, R] = Cont[A, R, R]
  @inline final def shift0[A, S, R](k: (A =>> S) =>> R): Cont[A, S, R] = Cont(done(k))
  @inline final def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = shift0(k => done(f(k(_).result)))
  @inline final def reset[A, R](k: Cont[A, A, R]): R = k.run(done).result
  @inline final def pure[A, R](a: A): A :#: R = shift0(_ (a))
  final case class Cont[A, S, R](cont: ![(A =>> S) =>> R]) extends AnyVal {
    @inline def run(f: A =>> S): ![R] = cont.flatMap(_ (a => tailcall(f(a))))
    @inline def bind[B, S1](f: A => (B =>> S1) =>> S): Cont[B, S1, R] = shift0(k => run(f(_)(k)))
    @inline def flatMap[B, S1](f: A => Cont[B, S1, S]): Cont[B, S1, R] = bind(a => f(a).run)
    @inline def map[B](f: A => B): Cont[B, S, R] = bind(a => _ (f(a)))
    @inline def >>=[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = flatMap(f)
    @inline def >>[B, C](c2: Cont[B, C, S]): Cont[B, C, R] = flatMap(_ => c2)
    @inline def >>>(s: S): Cont[S, S, R] = map(_ => s)
  }

  trait InCont[I] extends (OutCont[I] =>> Unit)
  @inline def InCont[I](k: OutCont[I] =>> Unit): InCont[I] = k(_)
  trait OutCont[O] extends (O => InCont[O] =>> Unit)
  @inline def OutCont[O](k: O => InCont[O] =>> Unit): OutCont[O] = k(_)
  type PipeCont[I, O] = InCont[I] => OutCont[O] =>> Unit
  type Pipe[I, O, A] = A :#: PipeCont[I, O]
  @inline def input[I, O]: Pipe[I, O, I] = shift(k => ki => ko =>
    ki(OutCont(i => k1 => tailcall(k(i)(k1)(ko)))))
  @inline def output[I, O](o: O): Pipe[I, O, Unit] = shift(k => ki => ko =>
    ko(o)(InCont(k1 => tailcall(k()(ki)(k1)))))
  @inline def merge[I, O, M, A](p: Pipe[I, M, A], q: Pipe[M, O, A]): Pipe[I, O, A] =
    shift(_ => ki => ko => q.run(_ => ???).flatMap(_ (InCont(
      k1 => p.run(_ => ???).flatMap(_ (ki)(k1))))(ko)))
  @inline def runPipe[I, O, A](p: Pipe[I, O, A]): PipeCont[I, O] =
    ki => ko => p.run(_ => done(_ => _ => done())).result(ki)(ko)

  trait Read[A] {
    def read(s: String): Option[A]
    @inline def readLine(r: BufferedReader): Option[A] = read(r.readLine())
  }

  object Read {
    @inline def apply[A: Read]: Read[A] = implicitly[Read[A]]
    implicit object readInt extends Read[Int] {
      @inline override def read(s: String): Option[Int] = Try(s.toInt).toOption
    }
  }

  def runPipeIO[I: Read, O](p: Pipe[I, O, Unit])(r: BufferedReader): Unit = {
    lazy val ki: InCont[I] = InCont(k => Read[I].readLine(r).fold(done())(k(_)(ki)))
    lazy val ko: OutCont[O] = OutCont { o => k => println(o); k(ko) }
    runPipe(p)(ki)(ko).result
  }

  //////

  def append[A](xs: List[A]): List[A] =>> List[A] = {
    def walk: List[A] =>> Cont[List[A], List[A], List[A] =>> List[A]] = {
      case List() => done(shift0(done))
      case h :: t => tailcall(walk(t).map(_.map(h :: _)))
    }

    reset(walk(xs).result)(_)
  }

  def double(): Pipe[Int, Int, Unit] = for {
    i <- input[Int, Int]
    _ <- output[Int, Int](i * 2)
    _ <- double()
  } yield ()

  def quad(): Pipe[Int, Int, Unit] = merge(double(), double())

  runPipeIO(quad())(new BufferedReader(new StringReader("1\n" * 10000)))

  println(append(List.fill(10000)(1))(List(5, 6, 7, 8)).result.length)
}
