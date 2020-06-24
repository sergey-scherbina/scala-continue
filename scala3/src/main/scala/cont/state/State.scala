package cont.state

import cont._

inline def state[A, S, R] (f: S => (A, S) ): A :#: (S => R) =
shift (k => s => Function.uncurried (k).tupled (f (s) ) )

inline def runState[S, R] (e: R :#: (S => R) ): S => R = e (a => _ => a)

inline def access[S, R] (f: S => S): S :#: (S => R) = state (s => (s, f (s) ) )

inline def get[S, R]: S :#: (S => R) = access (identity)

inline def set[S, R] (s: S): Unit :#: (S => R) = state (s => ((), s) )

inline def increase[R]: Int :#: (Int => R) = access (_ + 1)
