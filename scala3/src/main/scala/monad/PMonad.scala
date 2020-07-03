package monad

// Robert Atkey. Parameterised notions of computation. (2009)
// https://bentnib.org/paramnotions-jfp.html

// Monad M[S1,S2,A] is parameterised by an arrow S1 -> S2 in category of "states"
trait PMonad[M[_, _, _]] {

  // identity: S -> S
  def pure[S, A](a: A): M[S, S, A]

  // composition: S1 -> S2 o S2 -> S3 => S1 -> S3
  def flatMap[S1, S2, S3, A, B](m: M[S1, S2, A])
                               (f: (A => M[S2, S3, B])): M[S1, S3, B] =
    flatten(map(m)(f))

  def map[S1, S2, A, B](m: M[S1, S2, A])
                       (f: (A => B)): M[S1, S2, B] =
    flatMap(m)(x => pure(f(x)))

  def flatten[S1, S2, S3, A](m: M[S1, S2, M[S2, S3, A]]): M[S1, S3, A] =
    flatMap(m)(identity)

}

inline def pure[M[_, _, _]] (using M: PMonad[M] ) =[S, A] => (a: A) => M.pure[S, A] (a)

extension on[M[_, _, _], S1, S2, S3, A, B] (m: M[S1, S2, A] ) (using M: PMonad[M] ) {

  inline def flatMap (f: A => M[S2, S3, B] ): M[S1, S3, B] = M.flatMap (m) (f)

  inline def map (f: A => B): M[S1, S2, B] = M.map (m) (f)

  inline def foreach (f: A => Unit): Unit = M.map (m) (f)

}