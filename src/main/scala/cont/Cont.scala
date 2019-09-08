package cont

object Cont {

  type Cont[A, S, R] = (A => S) => R
  type :#:[A, R] = (A => R) => R

  @inline def pure[A, R](a: A): A :#: R = _ (a)

  @inline def bind[A, S1, R, B, S2](c: (A => S1) => R)(f: A => (B => S2) => S1): (B => S2) => R =
    k => c(f(_)(k))

  @inline def fmap[A, S, R, B](c: (A => S) => R)(f: A => B): (B => S) => R =
    bind(c)(a => pure(f(a)))

  implicit class ContMonad[A, S, R](val c: (A => S) => R) extends AnyVal {
    @inline def flatMap[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = bind(c)(f)

    @inline def >>=[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = bind(c)(f)

    @inline def map[B](f: A => B): Cont[B, S, R] = bind(c)(a => pure(f(a)))
  }

  implicit class ContLift[A, R](val m: (A => R) => R) extends AnyVal {
    @inline def lift[B]: A :#: B :#: R = bind(m)
  }

  @inline def shift0[A, S, R](e: (A => S) => R): Cont[A, S, R] = e

  @inline def reset0[A, R](c: Cont[A, A, R]): R = c(identity)

  @inline def shift1[A, B, R](e: (A => B :#: R) => B :#: R): A :#: B :#: R = e

  @inline def reset1[A, R](m: A :#: A :#: R): A :#: R = m(pure)

  def loop0[A, B](f: Cont[A, B, A => B]): A => B = f(loop0(f))(_)

  @inline def abort0[A, S, R](r: R): Cont[A, S, R] = shift0(_ => r)

  @inline def fail0[A, S]: Cont[A, S, Unit] = abort0()

  @inline def amb0[A, S](a: A, b: A): Cont[A, S, Unit] = shift0 { k => k(a); k(b); () }

  @inline def flip0[S]: Cont[Boolean, S, Unit] = amb0[Boolean, S](true, false)

  @inline def return0[A, B](a: A): A :#: B = shift0(pure(a))

  @inline def return1[A, B, R](a: A): A :#: B :#: R = shift1(pure(a))

  @inline def abort1[A, B, R](b: B): A :#: B :#: R = shift1(_ => pure(b))

  @inline def fail1[A, R]: A :#: Unit :#: R = abort1()

  @inline def amb1[A, R](a1: A, a2: A): A :#: Unit :#: R = shift1(k => for {_ <- k(a1); _ <- k(a2)} yield ())

  @inline def flip1[R]: Boolean :#: Unit :#: R = amb1[Boolean, R](true, false)

  @inline def emit0[A](a: A): Unit :#: List[A] = shift0(k => a :: k())

  @inline def emit1[A, R](a: A): Unit :#: List[A] :#: R = shift1(k => for (as <- k()) yield a :: as)

  @inline def collect0[A](m: Unit :#: List[A]): List[A] = reset0(for (_ <- m) yield List())

  @inline def collect1[A, R](m: Unit :#: List[A] :#: R): List[A] :#: R = reset1(for (_ <- m) yield List())

  @inline def emitZ0[A](a: A): Unit :#: LazyList[A] = shift0(k => a #:: k())

  @inline def emitZ1[A, R](a: A): Unit :#: LazyList[A] :#: R = shift1(k => for (as <- k()) yield a #:: as)

  @inline def collectZ0[A](m: Unit :#: LazyList[A]): LazyList[A] = reset0(for (_ <- m) yield LazyList())

  @inline def collectZ1[A, R](m: Unit :#: LazyList[A] :#: R): LazyList[A] :#: R = reset1(for (_ <- m) yield LazyList())

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
    @inline override def reflect0[A, B](m: List[A]): A :#: List[B] = shift0(m.flatMap(_))

    @inline override def reify0[A](m: A :#: List[A]): List[A] = m(List(_))

    @inline override def reflect1[A, B, R](m: List[A]): A :#: List[B] :#: List[R] =
      shift1(k1 => shift0(k2 => m.flatMap(k1(_)(k2))))

    @inline override def reify1[A, R](m: A :#: List[A] :#: R): List[A] :#: R = m(a => pure(List(a)))
  }

  implicit object LazyListReflection extends Reflection[LazyList] {
    @inline override def reflect0[A, B](m: LazyList[A]): A :#: LazyList[B] = shift0(m.flatMap(_))

    @inline override def reify0[A](m: A :#: LazyList[A]): LazyList[A] = m(LazyList(_))

    @inline def reflect1[A, B, R](m: LazyList[A]): A :#: LazyList[B] :#: LazyList[R] =
      shift1(k1 => shift0(k2 => m.flatMap(k1(_)(k2))))

    @inline override def reify1[A, R](m: A :#: LazyList[A] :#: R): LazyList[A] :#: R = m(a => pure(LazyList(a)))
  }

  @inline def project0[A, B, C](fa: A :#: B)(br: B => C)(rb: C => B): A :#: C =
    shift0((k: A => C) => br(fa(a => rb(k(a)))))

  @inline def project1[A, B, C](fa: A :#: B)(f1: B => C)(f2: C => B): A :#: B :#: C =
    project0[A, B, B :#: C](fa)(pure)(k2 => f2(k2(f1)))

  @inline def pair0[A, B, R](a1: A, a2: A): (A, A) :#: B =
    for (x <- return0[A, B](a1); y <- return0[A, B](a1)) yield (x, y)

  @inline def pair1[A, B, R](a1: A, a2: A): (A, A) :#: B :#: R =
    for (x <- return1[A, B, R](a1); y <- return1[A, B, R](a1)) yield (x, y)

  @inline def state0[A, S, R](f: S => (A, S)): A :#: (S => R) =
    shift0(k => s => Function.uncurried(k).tupled(f(s)))

  @inline def runState[S, R](e: R :#: (S => R)): S => R = e(k => _ => k)

  @inline def access0[A, S, R](f: S => S): S :#: (S => R) = state0(s => (s, f(s)))

  @inline def get0[S, R]: S :#: (S => R) = access0(identity)

  @inline def set0[S, R](s: S): Unit :#: (S => R) = state0(s => ((), s))

  @inline def increase0[R]: Int :#: (Int => R) = access0(_ + 1)

}
