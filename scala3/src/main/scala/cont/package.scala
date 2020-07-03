package cont

import scala.language.implicitConversions

type :#:[A, R] = Cont[R, R, A]

@inline def exit[A, B, R] (r: R) = shift ((k: A => B) => r)
@inline def abort[A, B] = exit[A, B, Unit] (() )

@inline def channel[A, B]: Cont[A => B, B, A] = shift ((k: A => B) => k)
def loop[A, B] (f: Cont[A => B, B, A] ): A => B = f (loop (f) ) (_)

@inline def delay[A, B] (a: => A): Cont[() => B, B, A] = shift ((k: A => B) => () => k (a) )
@inline def force[A] (a: () => A): A = a ()
@inline def par[A1, A2] (t: (() => A1, () => A2) ): (A1, A2) = (force (t._1), force (t._2) )

@inline def put[A] (a: A): A :#: LazyList[A] = shift (a #:: _ (a) )
@inline def end[A]: A :#: LazyList[A] = exit (LazyList.empty)

type Eff[E[_], A, B] = Req[E, A, B] | B

case class Req[E[_], A, B](eff: E[A], k: A => Eff[E, A, B])

inline def raise[E[_], A, B] (e: E[A] ): A :#: Eff[E, A, B] = shift (Req (e, _: A => Eff[E, A, B] ) )
  
def handle[E[_], A, B] (h: PartialFunction[E[A], A :#: Eff[E, A, B]] ): Eff[E, A, B] => Eff[E, A, B] =
_ match {
  case Req (eff: E[A], k: (A => Eff[E, A, B] ) ) =>
  if (h.isDefinedAt (eff) ) handle (h) (h (eff) (k) )
  else Req (eff, a => handle (h) (k (a) ) )
  case b: B => b
}
