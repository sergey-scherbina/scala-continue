package object monad {

  trait IxMonad[M[_, _, _]] {
    def pure[S, A](a: A): M[S, S, A]

    def bind[S1, S2, S3, A, B](m: M[S1, S2, A])(f: A => M[S2, S3, B]): M[S1, S3, B]

    def map[S1, S2, A, B](m: M[S1, S2, A])(f: A => B): M[S1, S2, B] = bind(m)(a => pure(f(a)))
  }

}
