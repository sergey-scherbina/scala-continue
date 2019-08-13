package cont

object Cont {

  type Cont[A, S, R] = (A => S) => R

  @inline def pure[A, R](a: A): Cont[A, R, R] = _ (a)

  @inline def bind[A, S1, R, B, S2](c: Cont[A, S1, R])(f: A => Cont[B, S2, S1]): Cont[B, S2, R] =
    k => c(f(_)(k))

  implicit class ContMonad[A, S1, R](val c: Cont[A, S1, R]) extends AnyVal {
    @inline def flatMap[B, S2](f: A => Cont[B, S2, S1]): Cont[B, S2, R] = bind(c)(f)

    @inline def >>=[B, S2](f: A => Cont[B, S2, S1]): Cont[B, S2, R] = bind(c)(f)

    @inline def map[B](f: A => B): Cont[B, S1, R] = bind(c)(a => pure(f(a)))

    @inline def lift[B, S2]: Cont[A, Cont[B, S2, S1], Cont[B, S2, R]] = bind(c)
  }

  @inline def shift0[A, S, R](e: (A => S) => R): Cont[A, S, R] = e

  @inline def shift1[A, S, R](e: (A => S) => R): Cont[A, Cont[A, R, S], Cont[A, R, R]] = bind(e)

  @inline def shift2[A, S1, R, B, S2](e: (A => S1) => (B => S2) => R): Cont[A, S1, Cont[B, S2, R]] = e

  @inline def reset0[A, R](c: Cont[A, A, R]): R = c(identity)

  @inline def reset1[A, R](c: Cont[A, Cont[A, R, R], Cont[A, R, R]]): Cont[A, R, R] = c(pure)

  @inline def reset2[A, S, R](c: Cont[A, A, Cont[S, S, R]]): R = reset0(reset0(c))

}
