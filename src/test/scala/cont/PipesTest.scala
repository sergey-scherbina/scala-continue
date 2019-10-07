package cont

import java.io.{BufferedReader, StringReader}
import cont.Cont._
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

    def double[A]: Pipe[Int, Int, A] = for (
      i <- input[Int, Int];
      _ <- output[Int, Int](i * 2);
      x <- double[A]) yield x

    def quad[A]: Pipe[Int, Int, A] = merge(double, double)

    Try(runPipeIO(quad)(
      new BufferedReader(new StringReader(
        "1\n" * 100
      ))))
  }
}
