package cont

import cont.Cont._
import scala.util.control.TailCalls._

object Pipes {
  trait InCont[I] extends (OutCont[I] =>> Unit)
  @inline def InCont[I](k: OutCont[I] =>> Unit): InCont[I] = k(_)
  trait OutCont[O] extends (O => InCont[O] =>> Unit)
  @inline def OutCont[O](k: O => InCont[O] =>> Unit): OutCont[O] = k(_)
  type PipeCont[I, O] = InCont[I] => OutCont[O] =>> Unit
  type Pipe[I, O, A] = A :#: PipeCont[I, O]

  @inline def input[I, O]: Pipe[I, O, I] = shift0(k =>
    done((ki: InCont[I]) => (ko: OutCont[O]) =>
      ki(OutCont(i => k1 => tailcall(k(i).flatMap(_ (k1)(ko)))))))

  @inline def output[I, O](o: O): Pipe[I, O, Unit] = shift0(k =>
    done((ki: InCont[I]) => (ko: OutCont[O]) =>
      ko(o)(InCont(k1 => tailcall(k().flatMap(_ (ki)(k1)))))))

  @inline def merge[I, O, M, A](p: Pipe[I, M, A], q: Pipe[M, O, A]): Pipe[I, O, A] =
    shift0(_ => done((ki: InCont[I]) => (ko: OutCont[O]) =>
      q(_ => ???).flatMap(_ (InCont(k1 =>
        p(_ => ???).flatMap(_ (ki)(k1))))(ko))))

  @inline def runPipe[I, O, A](p: Pipe[I, O, A]): PipeCont[I, O] =
    ki => ko => p(_ => done(_ => _ => done())).flatMap(_ (ki)(ko))

  def runPipeIO[I, O](p: Pipe[I, O, Unit])(read: => Option[I]): Unit = {
    lazy val ki: InCont[I] = InCont(k => read.fold(done())(k(_)(ki)))
    lazy val ko: OutCont[O] = OutCont { o => k => println(o); k(ko) }
    runPipe(p)(ki)(ko).result
  }
}
