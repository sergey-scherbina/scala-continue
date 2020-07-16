package cont

import monad._
export monad._
export monad.{given _}

type Cont[A, B, C] = (A => B) => C

inline def shift[A, B, C] (c: (A => B) => C): Cont[A, B, C] = c
inline def[A, B, C] (c: Cont[A, B, C] ) $: (A => B) => C = c
inline def reset[A, B] (c: Cont[A, A, B] ): B = c $ identity

given PMonad[Cont] {
  inline override def pure[A, B](a: A): Cont[A, B, B] = _ (a)

  inline override def flatMap[A, B, C1, C2, C3]
  (m: Cont[A, C2, C3])(f: A => Cont[B, C1, C2]): Cont[B, C1, C3] = k => m(f(_)(k))

  inline override def map[A, B, C1, C2]
  (m: Cont[A, C1, C2])(f: A => B): Cont[B, C1, C2] = k => m(k compose f)

  inline override def flatten[A, C1, C2, C3]
  (m: Cont[Cont[A, C1, C2], C2, C3]): Cont[A, C1, C3] = k => m(_ (k))
}

inline def[A, B1, B2] (f: Cont[A, B1, B2] )
withFilter (p: A => Boolean): Cont[A, B1, B2] = f
