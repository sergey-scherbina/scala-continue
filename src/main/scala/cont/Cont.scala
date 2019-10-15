package cont

import scala.util.control.TailCalls._

object Cont {

  type =>>[A, B] = A => TailRec[B]
  type Cont[A, S, R] = (A =>> S) =>> R
  type :#:[A, R] = Cont[A, R, R]

  final implicit class ContMonad[A, S, R](val c: (A =>> S) =>> R) extends AnyVal {
    @inline def run(f: A =>> S): TailRec[R] = tailcall(c(a => tailcall(f(a))))
    @inline def map[B](f: A => B): Cont[B, S, R] = (k => run(a => tailcall(k(f(a)))))
    @inline def flatMap[B, S1](f: A => Cont[B, S1, S]): Cont[B, S1, R] = (k => run(a => tailcall(f(a)(k))))
    @inline def >>=[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = flatMap(f)
    @inline def >>[B, C](c2: Cont[B, C, S]): Cont[B, C, R] = flatMap(_ => c2)
    @inline def >>>(s: S): Cont[S, S, R] = map(_ => s)
  }

  final implicit class ContLift[A, R](val c: A :#: R) extends AnyVal {
    @inline def lift[B]: A :#: B :#: R = (k => done(c >>= (k(_).result)))
  }

  @inline final def shift0[A, S, R](k: (A =>> S) =>> R): Cont[A, S, R] = k
  @inline final def reset0[A, R](k: Cont[A, A, R]): TailRec[R] = k.run(done)
  @inline final def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = shift0(k => done(f(k(_).result)))
  @inline final def reset[A, R](k: Cont[A, A, R]): R = reset0(k).result
  @inline final def shift1[A, B, R](e: (A =>> (B :#: R)) =>> (B :#: R)): A :#: B :#: R = shift0(e)
  @inline final def reset1[A, R](m: A :#: A :#: R): A :#: R = m.run(a => done(pure(a))).result
  @inline final def pure[A, R](a: A): A :#: R = shift0(_ (a))
  @inline def pure1[A, B, R](a: A): A :#: B :#: R = pure[A, R](a).lift[B]
  final def loop0[A, B](f: Cont[A, B, A =>> B]): A =>> B = f.run(loop0(f)).result
  @inline def abort0[A, S, R](r: R): Cont[A, S, R] = shift(_ => r)
  @inline def fail0[A, S]: Cont[A, S, Unit] = abort0()
  @inline def amb0[A, S](a: A, b: A): Cont[A, S, Unit] = shift0 { k => k(a); k(b); done() }
  @inline def flip0[S]: Cont[Boolean, S, Unit] = amb0[Boolean, S](true, false)
  @inline def abort1[A, B, R](b: B): A :#: B :#: R = shift1(_ => done(pure(b)))
  @inline def fail1[A, R]: A :#: Unit :#: R = abort1()
  @inline def amb1[A, R](a1: A, a2: A): A :#: Unit :#: R = shift1(k => for {_ <- k(a1); _ <- k(a2)} yield pure())
  @inline def flip1[R]: Boolean :#: Unit :#: R = amb1[Boolean, R](true, false)
  @inline def emit0[A](a: A): Unit :#: List[A] = shift0(_ ().map(a :: _))
  @inline def emit1[A, R](a: A): Unit :#: List[A] :#: R = shift1(k => for (as <- k()) yield as.map(a :: _))
  @inline def collect0[A](m: Unit :#: List[A]): List[A] = reset(for (_ <- m) yield List[A]())
  @inline def collect1[A, R](m: Unit :#: List[A] :#: R): List[A] :#: R = reset1(for (_ <- m) yield List())
  @inline def emitS0[A](a: A): Unit :#: Stream[A] = shift0(_ ().map(a #:: _))
  @inline def emitS1[A, R](a: A): Unit :#: Stream[A] :#: R = shift1(k => for (as <- k()) yield as.map(a #:: _))
  @inline def collectS0[A](m: Unit :#: Stream[A]): Stream[A] = reset(for (_ <- m) yield Stream[A]())
  @inline def collectS1[A, R](m: Unit :#: Stream[A] :#: R): Stream[A] :#: R = reset1(for (_ <- m) yield Stream())
  @inline def pair0[A, B](a1: A, a2: A): (A, A) :#: B = for (x <- pure[A, B](a1); y <- pure[A, B](a1)) yield (x, y)
  @inline def pair1[A, B, R](a1: A, a2: A): (A, A) :#: B :#: R = pair0[A, R](a1, a2).lift[B]
  @inline def state0[A, S, R](f: S => (A, S)): A :#: (S => R) =
    shift0(k => done((s: S) => (k(_: A).result(_: S)).tupled(f(s))))
  @inline def runState[S, R](e: R :#: (S => R)): S => R = e.run(k => done(_ => k)).result
  @inline def access0[A, S, R](f: S => S): S :#: (S => R) = state0(s => (s, f(s)))
  @inline def get0[S, R]: S :#: (S => R) = access0(identity)
  @inline def set0[S, R](s: S): Unit :#: (S => R) = state0(s => ((), s))
  @inline def increase0[R]: Int :#: (Int => R) = access0(_ + 1)

  //  @inline def project0[A, B, C](fa: A :#: B)(br: B => C)(rb: C => B): A :#: C =
  //    shift0((k: A => C) => br(fa(a => rb(k(a)))))
  //
  //  @inline def project1[A, B, C](fa: A :#: B)(f1: B => C)(f2: C => B): A :#: B :#: C =
  //    project0[A, B, B :#: C](fa)(pure)(k2 => f2(k2(f1)))

  trait Reflection[F[_]] {
    def reflect0[A, B](m: F[A]): A :#: F[B]
    def reflect1[A, B, R](m: F[A]): A :#: F[B] :#: F[R]
    def reify0[A](m: A :#: F[A]): F[A]
    def reify1[A, R](m: A :#: F[A] :#: R): F[A] :#: R
  }

  implicit class Reflect0[A, F[_] : Reflection](m: F[A]) {
    def reflect0[B]: A :#: F[B] = implicitly[Reflection[F]].reflect0(m)
    def reflect1[B, R](m: F[A]): A :#: F[B] :#: F[R] = implicitly[Reflection[F]].reflect1(m)
  }

  implicit class Reify0[A, F[_] : Reflection](m: A :#: F[A]) {
    def reify0: F[A] = implicitly[Reflection[F]].reify0(m)
  }

  implicit class Reify1[A, R, F[_] : Reflection](m: A :#: F[A] :#: R) {
    def reify1: F[A] :#: R = implicitly[Reflection[F]].reify1(m)
  }

  implicit object ListReflection extends Reflection[List] {
    @inline override def reflect0[A, B](m: List[A]): A :#: List[B] = shift0(k => done(m.flatMap(k(_).result)))
    @inline override def reify0[A](m: A :#: List[A]): List[A] = m.run(a => done(List(a))).result
    @inline override def reflect1[A, B, R](m: List[A]): A :#: List[B] :#: List[R] = shift1(k1 => done(shift0(k2 =>
      done(m.flatMap(k1(_).result.run(k2).result)))))
    @inline override def reify1[A, R](m: A :#: List[A] :#: R): List[A] :#: R = m.run(a => done(pure(List(a)))).result
  }

  implicit object StreamReflection extends Reflection[Stream] {
    @inline override def reflect0[A, B](m: Stream[A]): A :#: Stream[B] = shift0(k => done(m.flatMap(k(_).result)))
    @inline override def reify0[A](m: A :#: Stream[A]): Stream[A] = m.run(a => done(Stream(a))).result
    @inline def reflect1[A, B, R](m: Stream[A]): A :#: Stream[B] :#: Stream[R] = shift1(k1 => done(shift0(k2 =>
      done(m.flatMap(k1(_).result.run(k2).result)))))
    @inline override def reify1[A, R](m: A :#: Stream[A] :#: R): Stream[A] :#: R = m.run(a => done(pure(Stream(a)))).result
  }

}
