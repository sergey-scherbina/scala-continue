package monad

package object cont {

  type Cont[R, B, A] = (A => B) => R

  def pure[S, A](a: A): Cont[S, S, A] = _ (a)

  def reset[R, A](c: Cont[R, A, A]): R = c(identity)

  def shift[R, B, A](f: (A => B) => R): Cont[R, B, A] = f(_)

  def take[A, B]: Cont[A => B, B, A] = shift(identity)

  def loop[A, B](f: Cont[A => B, B, A]): A => B = f(loop(f))(_)

  def stop[A, B](a: A): Cont[A, A, B] = shift(_ => a)

  def put[A](a: A): Cont[Stream[A], Stream[A], A] = shift(a #:: _ (a))

  def pipe[A]: Cont[A => Stream[A], Stream[A], A] = take[A, Stream[A]] >>= put

  def lift[A](f: A => A): Cont[A => Stream[A], Stream[A], A] = pipe[A] map f

  def repeat[A](a: A): Stream[A] = loop(pipe[A])(a)

  def gen[A](f: A => A): A => Stream[A] = loop(lift(f))

  def unfold[A](a: A)(f: A => A): Stream[A] = gen(f)(a)

  def suspend[A, B](b: B): Cont[B << A, B << A, A] = shift(<<(b, _))

  def channel[A, B]: Cont[B => B << A, B << A, A] = take[B, B << A] >>= suspend

  def reflect[M[_] : Monad, A, B](m: M[A]): Cont[M[B], M[B], A] = shift(Monad[M].flatMap(m))

  def reify[M[_] : Monad, A, B](e: Cont[M[B], M[A], A]): M[B] = e(Monad[M].pure)

  implicit class MReflect[M[_] : Monad, A](m: M[A]) {
    def reflect[B]: Cont[M[B], M[B], A] = shift(Monad[M].flatMap(m))
  }

  case class <<[A, B](get: A, put: B => A << B) {
    def apply(): A = get

    def apply(b: B): A << B = put(b)

    def <<(f: A => B): Stream[A] = get #:: (put(f(get)) << f)

    def toStream[C >: A <: B]: Stream[C] = <<(identity[C])

    def forAll(b: B): Stream[A] = <<(_ => b)
  }

  object ContIndexedMonad extends IndexedMonad[Cont] {
    override def pure[S, A](a: A): Cont[S, S, A] = _ (a)

    override def bind[S1, S2, S3, A, B](m: Cont[S1, S2, A])(
      f: A => Cont[S2, S3, B]): Cont[S1, S3, B] = k => m(f(_)(k))
  }

  implicit class ContMonad[R1, R2, A](val c: Cont[R1, R2, A]) extends AnyVal {
    def map[B](f: A => B): Cont[R1, R2, B] = ContIndexedMonad.map(c)(f)

    def flatMap[R3, B](f: A => Cont[R2, R3, B]): Cont[R1, R3, B] = ContIndexedMonad.bind(c)(f)

    def >>=[R3, B](f: A => Cont[R2, R3, B]): Cont[R1, R3, B] = ContIndexedMonad.bind(c)(f)

    def withFilter(f: A => Boolean): ContMonad[R1, R2, A] = this // TODO
  }

}
