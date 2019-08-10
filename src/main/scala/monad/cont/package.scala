package monad

package object cont {

  type Cont[A, B, C] = (A => B) => C

  @inline def pure[A, R](a: A): Cont[A, R, R] = _ (a)

  @inline def bind[A, B, R, S1, S2](m: Cont[A, S1, R])(f: A => Cont[B, S2, S1]): Cont[B, S2, R] =
    k => m(f(_)(k))

  @inline def shift0[A, B, C](f: (A => B) => C): Cont[A, B, C] = f(_)

  @inline def reset0[A, B](c: Cont[A, A, B]): B = c(identity)

  @inline def shift1[A, B, C, D](f: (A => B) => Cont[D, D, C]): Cont[A, B, C] =
    shift0(k => reset0(f(k)))

  @inline def reset1[A, B, C](e: Cont[A, A, B]): Cont[B, C, C] = pure(reset0(e))

  @inline def shift2[A, B, C, D, E](c: (A => B) => (C => D) => E): Cont[A, B, Cont[C, D, E]] =
    shift0((k1: A => B) => shift0((k2: C => D) => c(k1)(k2)))

  @inline def reset2[A, B, C](c: Cont[A, A, Cont[B, B, C]]): C = reset0(reset0(c))

  @inline def take[A, B]: Cont[A, B, A => B] = shift0(identity)

  def loop[A, B](f: Cont[A, B, A => B]): A => B = f(loop(f))(_)

  @inline def stop[A, B](a: A): Cont[B, B, A] = shift0(_ => a)

  @inline def put[A](a: A): Cont[A, Stream[A], Stream[A]] = shift0(a #:: _ (a))

  @inline def pipe[A]: Cont[A, Stream[A], A => Stream[A]] = take[A, Stream[A]] >>= put[A]

  @inline def lift[A](f: A => A): Cont[A, Stream[A], A => Stream[A]] = pipe[A] map f

  @inline def repeat[A](a: A): Stream[A] = loop(pipe[A])(a)

  @inline def gen[A](f: A => A): A => Stream[A] = loop(lift(f))

  @inline def unfold[A](a: A)(f: A => A): Stream[A] = gen(f)(a)

  @inline def suspend[A, B](a: A): Cont[B, A << B, A << B] = shift0(<<(a, _))

  @inline def channel[A, B]: Cont[A, B << A, B => B << A] = take[B, B << A] >>= suspend

  @inline def reflect[M[_] : Monad, A, B](m: M[A]): Cont[A, M[B], M[B]] = shift0(Monad[M].flatMap(m))

  @inline def reify[M[_] : Monad, A, B](e: Cont[A, M[A], B]): B = e(Monad[M].pure)

  implicit class Reflect[M[_] : Monad, A](m: M[A]) {
    def reflect[B]: Cont[A, M[B], M[B]] = cont.reflect(m)
  }

  case class <<[A, B](get: A, put: B => A << B) {
    @inline def apply(): A = get

    @inline def apply(b: B): A << B = put(b)

    def <<(f: A => B): Stream[A] = get #:: (put(f(get)) << f)

    @inline def toStream[C >: A <: B]: Stream[C] = <<(identity[C])

    @inline def forAll(b: B): Stream[A] = <<(_ => b)
  }

  object ContIndexedMonad extends IndexedMonad[Cont] {
    @inline override def pure[A, R](a: A): Cont[A, R, R] = cont.pure(a)

    @inline override def bind[A, S, R, B, S2](m: Cont[A, S, R])(
      f: A => Cont[B, S2, S]): Cont[B, S2, R] = cont.bind(m)(f)
  }

  implicit class ContMonad[A, S, R](val c: Cont[A, S, R]) extends AnyVal {
    @inline def map[B](f: A => B): Cont[B, S, R] = ContIndexedMonad.map(c)(f)

    @inline def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = ContIndexedMonad.bind(c)(f)

    @inline def >>=[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = ContIndexedMonad.bind(c)(f)

    @inline def withFilter(f: A => Boolean): ContMonad[A, S, R] = this // TODO
  }

}
