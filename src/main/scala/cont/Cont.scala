package cont

import com.sun.org.apache.xpath.internal.operations.Bool

object Cont {

  type Cont[A, S, R] = (A => S) => R
  type :#:[A, R] = Cont[A, R, R]
  type Stm[R, A] = Cont[A, R, R]

  @inline def pure[A, R](a: A): Cont[A, R, R] = _ (a)

  @inline def bind[A, S1, R, B, S2](c: Cont[A, S1, R])(f: A => Cont[B, S2, S1]): Cont[B, S2, R] =
    k => c(f(_)(k))

  implicit class ContMonad[A, S1, R](val c: Cont[A, S1, R]) extends AnyVal {
    @inline def flatMap[B, S2](f: A => Cont[B, S2, S1]): Cont[B, S2, R] = bind(c)(f)

    @inline def >>=[B, S2](f: A => Cont[B, S2, S1]): Cont[B, S2, R] = bind(c)(f)

    @inline def map[B](f: A => B): Cont[B, S1, R] = bind(c)(a => pure(f(a)))

    @inline def lift[B, S2]: Cont[A, Cont[B, S2, S1], Cont[B, S2, R]] = bind(c)
  }

  implicit class StmMonad[RS, A](val m: Stm[RS, A]) extends AnyVal {
    @inline def flatMap[B](f: A => Stm[RS, B]): Stm[RS, B] = bind(m: Cont[A, RS, RS])(f)

    @inline def map[B](f: A => B): Stm[RS, B] = bind(m: Cont[A, RS, RS])(a => pure(f(a)))

    @inline def lift[R]: Stm[R :#: RS, A] = bind(m)
  }

  @inline def reset0[A, R](c: Cont[A, A, R]): R = c(identity)

  @inline def reset1[A, R](m: Stm[A :#: R, A]): Stm[R, A] = m(pure)

  @inline def reset2[A, S, R](c: Cont[A, A, Cont[S, S, R]]): R = reset0(reset0(c))

  @inline def shift0[A, S, R](e: (A => S) => R): Cont[A, S, R] = e

  @inline def shift1[A, R, RS](e: (A => Stm[RS, R]) => Stm[RS, R]): Stm[R :#: RS, A] = e

  @inline def shift2[A, S1, R, B, S2](e: (A => S1) => (B => S2) => R): Cont[A, S1, Cont[B, S2, R]] = e

  @inline def shift[A, R, RS](e: (A => Stm[R :#: RS, R]) => Stm[R :#: RS, R]): Stm[R :#: RS, A] =
    shift0(k => reset1(e(a => k(a).lift)))

  @inline def abort0[A, R](r: R): Stm[R, A] = shift0(_ => r)

  @inline def fail0[A]: Stm[Unit, A] = abort0()

  @inline def abort1[A, R, RS](r: R): Stm[R :#: RS, A] = shift0(_ => pure(r))

  @inline def fail1[A, RS]: Stm[Unit :#: RS, A] = abort1()

  @inline def amb0[A, R](a: A, b: A): Stm[R, A] = shift0 { k => k(a); k(b) }

  @inline def flip0[R]: Stm[R, Boolean] = amb0[Boolean, R](true, false)

  @inline def amb1[A, R, RS](a: A, b: A): Stm[R :#: RS, A] =
    shift0(k => for {_ <- k(a); r <- k(b)} yield r)

  @inline def flip1[R, RS]: Stm[R :#: RS, Boolean] = amb1[Boolean, R, RS](true, false)

  @inline def emit0[A](a: A): Stm[List[A], Unit] = shift0(k => a :: k())

  @inline def emit1[A, R](a: A): Stm[List[A] :#: R, Unit] =
    shift0(k => for (as <- k()) yield a :: as)

  @inline def collect0[A](m: Stm[List[A], Unit]): List[A] =
    reset0(for (_ <- m) yield List())

  @inline def collect1[A, R](m: Stm[List[A] :#: R, Unit]): Stm[R, List[A]] =
    reset1(for (_ <- m) yield List())

}
