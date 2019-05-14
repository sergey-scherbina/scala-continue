package object monad {

  trait IndexedMonad[M[_, _, _]] {
    def pure[S, A](a: A): M[S, S, A]

    def bind[S1, S2, S3, A, B](m: M[S1, S2, A])(f: A => M[S2, S3, B]): M[S1, S3, B]

    def map[S1, S2, A, B](m: M[S1, S2, A])(f: A => B): M[S1, S2, B] =
      bind(m)(a => pure(f(a)))
  }

  trait Monad[M[_]] {
    def pure[A](a: A): M[A]

    def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]

    def map[A, B](m: M[A])(f: A => B): M[B] =
      flatMap(m)(a => pure(f(a)))
  }

  object Monad {
    def apply[M[_] : Monad]: Monad[M] = implicitly

    implicit object OptionMonad extends Monad[Option] {
      override def pure[A](a: A): Option[A] = Option(a)

      override def flatMap[A, B](m: Option[A])(f: A => Option[B]): Option[B] =
        m.flatMap(f)
    }

    implicit object ListMonad extends Monad[List] {
      override def pure[A](a: A): List[A] = List(a)

      override def flatMap[A, B](m: List[A])(f: A => List[B]): List[B] =
        m.flatMap(f)
    }

  }

  trait Reflection[M[_]] {
    def reflect[A](e: M[A]): A

    def reify[A](e: => A): M[A]
  }

}
