package monad

package object cont {

  type Cont[A, B, C] = (A => B) => C

  def pure[A, R](a: A): Cont[A, R, R] = _ (a)

  def bind[A, B, R, S1, S2](m: Cont[A, S1, R])(f: A => Cont[B, S2, S1]): Cont[B, S2, R] =
    k => m(f(_)(k))

  def shift0[A, B, C](f: (A => B) => C): Cont[A, B, C] = f(_)

  def reset0[A, B](c: Cont[A, A, B]): B = c(identity)

  def shift1[A, B, C, D](f: (A => B) => Cont[D, D, C]): Cont[A, B, C] =
    shift0(k => reset0(f(k)))

  def reset1[A, B, C](e: Cont[A, A, B]): Cont[B, C, C] = pure(reset0(e))

  def take[A, B]: Cont[A, B, A => B] = shift0(identity)

  def loop[A, B](f: Cont[A, B, A => B]): A => B = f(loop(f))(_)

  def stop[A, B](a: A): Cont[B, B, A] = shift0(_ => a)

  def put[A](a: A): Cont[A, Stream[A], Stream[A]] = shift0(a #:: _ (a))

  def pipe[A]: Cont[A, Stream[A], A => Stream[A]] = take[A, Stream[A]] >>= put[A]

  def lift[A](f: A => A): Cont[A, Stream[A], A => Stream[A]] = pipe[A] map f

  def repeat[A](a: A): Stream[A] = loop(pipe[A])(a)

  def gen[A](f: A => A): A => Stream[A] = loop(lift(f))

  def unfold[A](a: A)(f: A => A): Stream[A] = gen(f)(a)

  def suspend[A, B](a: A): Cont[B, A << B, A << B] = shift0(<<(a, _))

  def channel[A, B]: Cont[A, B << A, B => B << A] = take[B, B << A] >>= suspend

  def reflect[M[_] : Monad, A, B](m: M[A]): Cont[A, M[B], M[B]] = shift0(Monad[M].flatMap(m))

  def reify[M[_] : Monad, A, B](e: Cont[A, M[A], B]): B = e(Monad[M].pure)

  implicit class Reflect[M[_] : Monad, A](m: M[A]) {
    def reflect[B]: Cont[A, M[B], M[B]] = cont.reflect(m)
  }

  case class <<[A, B](get: A, put: B => A << B) {
    def apply(): A = get

    def apply(b: B): A << B = put(b)

    def <<(f: A => B): Stream[A] = get #:: (put(f(get)) << f)

    def toStream[C >: A <: B]: Stream[C] = <<(identity[C])

    def forAll(b: B): Stream[A] = <<(_ => b)
  }

  object ContIndexedMonad extends IndexedMonad[Cont] {
    override def pure[A, R](a: A): Cont[A, R, R] = cont.pure(a)

    override def bind[A, S, R, B, S2](m: Cont[A, S, R])(
      f: A => Cont[B, S2, S]): Cont[B, S2, R] = cont.bind(m)(f)
  }

  implicit class ContMonad[A, S, R](val c: Cont[A, S, R]) extends AnyVal {
    def map[B](f: A => B): Cont[B, S, R] = ContIndexedMonad.map(c)(f)

    def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = ContIndexedMonad.bind(c)(f)

    def >>=[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = ContIndexedMonad.bind(c)(f)

    def withFilter(f: A => Boolean): ContMonad[A, S, R] = this // TODO
  }

}
