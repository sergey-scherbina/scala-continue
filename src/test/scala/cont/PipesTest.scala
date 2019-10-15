package cont

import java.io.{BufferedReader, StringReader}

import org.scalatest.FunSuite

import scala.util.Try
import cont.Pipes._
import cont.Cont._

class PipesTest extends FunSuite {

  test("pipes") {
    def double(): Pipe[Int, Int, Unit] = for {
      i <- input[Int, Int]
      _ <- output[Int, Int](i * 2)
      _ <- double()
    } yield ()

    def quad(): Pipe[Int, Int, Unit] = merge(double(), double())

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

    def runPipeUnsafe[I: Read, O](p: Pipe[I, O, Unit])(r: BufferedReader): Unit =
      runPipeIO(p)(Read[I].readLine(r))

    runPipeUnsafe(quad())(new BufferedReader(new StringReader("1\n" * 10000)))
  }
}
