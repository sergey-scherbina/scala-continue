package monad

// Robert Atkey. Parameterised notions of computation. (2009)
// https://bentnib.org/paramnotions-jfp.html

// Monad M[A, S1, S2] represents a computation of value A 
// wich changes a state from S1 to S2, i.e. it's indexed by 
// an arrow S1 -> S2 in a category S of "states"
trait PMonad[M[_, _, _]] {

  // identity: S -> S
  def pure[A, S](a: A): M[A, S, S]

  // composition: (S2 -> S3) o (S1 -> S2) = S1 -> S3
  def flatMap[A, B, S1, S2, S3](m: M[A, S2, S3])
                               (f: (A => M[B, S1, S2])): M[B, S1, S3] =
    flatten(map(m)(f))

  // composition: (S1 -> S2) o (S2 -> S3) = S1 -> S3
  def flatten[A, S1, S2, S3](m: M[M[A, S1, S2], S2, S3]): M[A, S1, S3] =
    flatMap(m)(identity)

  def map[A, B, S1, S2](m: M[A, S1, S2])(f: (A => B)): M[B, S1, S2] =
    flatMap(m)(x => pure(f(x)))

}

inline def pure[M[_, _, _]] (using M: PMonad[M] ) =[S, A] => (a: A) => M.pure[A, S] (a)

extension on[M[_, _, _], S1, S2, S3, A, B] (m: M[A, S2, S3] ) (using M: PMonad[M] ) {

  inline def flatMap (f: A => M[B, S1, S2] ): M[B, S1, S3] = M.flatMap (m) (f)

  inline def map (f: A => B): M[B, S2, S3] = M.map (m) (f)

  inline def foreach (f: A => Unit): Unit = M.map (m) (f)

}