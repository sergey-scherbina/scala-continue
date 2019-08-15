package cont

object Cont {

  type Cont[A, S, R] = (A => S) => R
  type :#:[A, R] = Cont[A, R, R]

  @inline def pure[A, R](a: A): Cont[A, R, R] = _ (a)

  @inline def bind[A, S1, R, B, S2](c: Cont[A, S1, R])(f: A => Cont[B, S2, S1]): Cont[B, S2, R] =
    k => c(f(_)(k))

  implicit class ContMonad[A, S, R](val c: Cont[A, S, R]) extends AnyVal {
    @inline def flatMap[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = bind(c)(f)

    @inline def >>=[B, C](f: A => Cont[B, C, S]): Cont[B, C, R] = bind(c)(f)

    @inline def map[B](f: A => B): Cont[B, S, R] = bind(c)(a => pure(f(a)))
  }

  implicit class CpsMonad[A, R](val m: A :#: R) extends AnyVal {
    @inline def flatMap[B](f: A => B :#: R): B :#: R = bind(m: Cont[A, R, R])(f)

    @inline def >>=[B](f: A => B :#: R): B :#: R = bind(m: Cont[A, R, R])(f)

    @inline def map[B](f: A => B): B :#: R = bind(m: Cont[A, R, R])(a => pure(f(a)))

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

  @inline def abort1[A, B, R](b: B): A :#: B :#: R = shift1(_ => pure(b))

  @inline def fail1[A, R]: A :#: Unit :#: R = abort1()

  @inline def amb1[A, R](a1: A, a2: A): A :#: Unit :#: R = shift1(k => for {_ <- k(a1); b <- k(a2)} yield ())

  @inline def flip1[R]: Boolean :#: Unit :#: R = amb1[Boolean, R](true, false)

  @inline def emit0[A](a: A): Unit :#: List[A] = shift0(k => a :: k())

  @inline def emit1[A, R](a: A): Unit :#: List[A] :#: R = shift1(k => for (as <- k()) yield a :: as)

  @inline def collect0[A](m: Unit :#: List[A]): List[A] = reset0(for (_ <- m) yield List())

  @inline def collect1[A, R](m: Unit :#: List[A] :#: R): List[A] :#: R = reset1(for (_ <- m) yield List())

}
