package cont

import monad._
export monad._
export monad.{given _}

type Cont[R1, R2, A] = (A => R2) => R1

inline def shift[A, B, R] (c: (A => B) => R): Cont[R, B, A] = c
inline def[A, B, R] (c: Cont[R, B, A] ) $: (A => B) => R = c
inline def reset[A, R] (c: Cont[R, A, A] ): R = c $ identity

given PMonad[Cont] {
  inline override def pure[R, A](a: A): Cont[R, R, A] = _ (a)

  inline override def flatMap[R1, R2, R3, A, B]
  (m: Cont[R1, R2, A])(f: A => Cont[R2, R3, B]): Cont[R1, R3, B] = k => m(f(_)(k))

  inline override def map[R1, R2, A, B]
  (m: Cont[R1, R2, A])(f: A => B): Cont[R1, R2, B] = k => m(k compose f)

  inline override def flatten[R1, R2, R3, A]
  (m: Cont[R1, R2, Cont[R2, R3, A]]): Cont[R1, R3, A] = k => m(_ (k))
}

inline def[R1, R2, A] (f: Cont[R1, R2, A] )
withFilter (p: A => Boolean): Cont[R1, R2, A] = f
