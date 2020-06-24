package cont

import scala.language.implicitConversions

inline def channel[A, B]: Cont[A => B, B, A] = shift ((k: A => B) => k)

def loop[A, B] (f: Cont[A => B, B, A] ): A => B = f (loop (f) ) (_)

inline def abort[A, B, R] (r: R): Cont[R, B, A] = shift ((k: A => B) => r)

inline def raise[A, B, R] (r: R) = shift ((k: A => B) => (r, k) )

inline def fail[A, B] = abort[A, B, Unit] (() )

inline def put[A] (a: A): A :#: LazyList[A] = shift (a #:: _ (a) )

inline def end[A]: A :#: LazyList[A] = abort (LazyList.empty)

def delay[A, B] (a: => A) = shift ((k: A => B) => () => k (a) )

def par[A1, A2] (t: (() => A1, () => A2) ): (A1, A2) = (t._1 (), t._2 () )
