package cont

import scala.language.implicitConversions

type :#:[A, B] = Cont[A, B, B]

inline def exit[A, B, R] (r: R) = shift ((k: A => B) => r)
inline def abort[A, B] () = exit[A, B, Unit] (() )
inline def pass[A, R] (a: A): A :#: R = shift ((k: A => R) => k (a) )
inline def continue[R] (): Unit :#: R = pass (() )

inline def channel[A, B]: Cont[A, B, A => B] = shift ((k: A => B) => k)
def loop[A, B] (f: Cont[A, B, A => B] ): A => B = f (loop (f) ) (_)

@inline def delay[A, B] (a: => A): Cont[A, B, () => B] = shift ((k: A => B) => () => k (a) )
inline def force[A] (a: () => A): A = a ()
inline def par[A1, A2] (t: (() => A1, () => A2) ): (A1, A2) = (force (t._1), force (t._2) )

inline def put[A] (a: A): A :#: LazyList[A] = shift (a #:: _ (a) )
inline def end[A]: A :#: LazyList[A] = exit (LazyList.empty)

inline def state[A, S, R] (f: S => (A, S) ): A :#: (S => R) = shift (k =>
s => Function.uncurried (k).tupled (f (s) ) )

inline def runState[S, R] (e: R :#: (S => R) ): S => R = e (a => _ => a)
inline def update[S, R] (f: S => S): S :#: (S => R) = state (s => (s, f (s) ) )
inline def get[S, R]: S :#: (S => R) = update (identity)
inline def set[S, R] (s: S): Unit :#: (S => R) = state (s => ((), s) )
inline def increase[R]: Int :#: (Int => R) = update (_ + 1)

case class Req[E[_], A, B](eff: E[A], k: A => B | Req[_, _, B])

type Eff[E[_], A, B] = B | Req[E, A, B]
type Raise[E[_], A, B] = A :#: Eff[E, A, B]
type :|:[E[_], F[_]] =[A] =>> E[A] | F[A]

inline def raise[E[_], A, B] (e: E[A] ): Raise[E, A, B] = shift (Req (e, _) )

type EffHandler[E[_], A, B, W] = (W, Eff[E, A, B] ) => (W, Eff[E, A, B] )

def handler0[E[_], A, B, W] (h: (W, E[A] ) => Option[(W, Raise[E, A, B] )] )
: ((W, Eff[E, A, B] ) ) => (W, Eff[E, A, B] ) = {

@annotation.tailrec def go (eff: (W, Eff[E, A, B] ) ): (W, Eff[E, A, B] ) = eff match {
case (w: W, Req (e: E[A], k: (A => Eff[E, A, B] ) ) ) => h (w, e) match {
case Some ((w2, e) ) => go (w2, e (k) )
case _ => (w, Req (e, a => handler0 (h) ((w, k (a) ) )._2) )
}
case b => b
}

go
}

def[E[_], A, B, W] (h1: EffHandler[E, A, B, W] ) :|: (h2: EffHandler[E, A, B, W] ): EffHandler[E, A, B, W] =
Function.untupled (Function.tupled (h1).compose (Function.tupled (h2) ) )

inline def stateHandler[E[_], A, B, W] (h: PartialFunction[(W, E[A] ),
(W, Raise[E, A, B] )] ): EffHandler[E, A, B, W] = (w, eff) =>
handler0 (Function.untupled (h.lift) ) ((w, eff) )

inline def handler[E[_], A, B] (h: PartialFunction[E[A], Raise[E, A, B]] ): Eff[E, A, B] => Eff[E, A, B] =
eff => handler0 ((w, e: E[A] ) => h.lift (e).map ((w, _) ) ) ((), eff)._2

inline def process_[E[_], A, B, W] (p: W => W :#: Eff[E, A, B] ): W => Eff[E, A, B] =
loop (for {
w <- channel[W, Eff[E, A, B]]
w <- p (w)
} yield w)

inline def process[E[_], A, B] (p: Unit :#: Eff[E, A, B] ): Eff[E, A, B] = process_ (_ => p) (() )
