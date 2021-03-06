package cont

import cont.Cont._
import scala.util.control.TailCalls._

object Pipes {

  trait InCont[I] extends (OutCont[I] =>> Unit)
  @inline def InCont[I](k: InCont[I]): InCont[I] = k
  trait OutCont[O] extends (O => InCont[O] =>> Unit)
  @inline def OutCont[O](k: OutCont[O]): OutCont[O] = k
  type PipeCont[I, O] = InCont[I] => OutCont[O] =>> Unit
  type Pipe[I, O, A] = A :#: PipeCont[I, O]

  @inline def input[I, O]: Pipe[I, O, I] = shift(k => ki => ko =>
    ki(OutCont(i => k1 => tailcall(k(i)(k1)(ko)))))

  @inline def output[I, O](o: O): Pipe[I, O, Unit] = shift(k => ki => ko =>
    ko(o)(InCont(k1 => tailcall(k()(ki)(k1)))))

  @inline def merge[I, O, M, A](p: Pipe[I, M, A], q: Pipe[M, O, A]): Pipe[I, O, A] =
    shift(_ => ki => ko => q(_ => ???).flatMap(_ (InCont(k1 => p(_ => ???)
      .flatMap(_ (ki)(k1))))(ko)))

  @inline def runPipe[I, O, A](p: Pipe[I, O, A]): PipeCont[I, O] =
    ki => ko => p(_ => done(_ => _ => done())).flatMap(_ (ki)(ko))

  def runPipeIO[I, O](p: Pipe[I, O, Unit])(read: => Option[I]): Unit = {
    lazy val ki: InCont[I] = InCont(k => read.fold(done())(k(_)(ki)))
    lazy val ko: OutCont[O] = OutCont { o => k =>
      println(o);
      k(ko)
    }
    runPipe(p)(ki)(ko).result
  }
}
