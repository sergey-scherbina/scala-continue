package effects

import cont._

type :@:[A] = Unit
type :||:[E[_], F[_]] =[A] =>> (E[A], F[A] ) match {
case (Unit, _) => F[A]
case (_, Unit) => E[A]
case _ => E[A] | F[A]
}

enum Ef[E[_], A] {

  case Ret[A](a: A) extends Ef[:@:, A]

  case Req[E[_], F[_], A, B](e: E[A] | F[A], k: A => Ef[F, B])
    extends Ef[E :||: F, B]

}

import Ef._

inline implicit def ret[A] (a: A): Ef[:@:, A] = Ret (a)
inline def req[E[_], F[_], A, B] (e: E[A] ): Cont[A, Ef[F, B], Ef[E :||: F, B]] = shift (Ef.Req (e, _) )
def effect[E[_], A] (e: E[A] ) =[F[_], B] => () => req[E, F, A, B] (e)

