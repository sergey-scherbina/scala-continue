package cont

import cont.Cont._
import org.scalatest.FunSuite

class ReflectTest extends FunSuite {
  test("reflect list") {


    @inline def reflect01[A, B, R](m: List[A] :#: R): (A :#: List[B]) :#: R = m.map(reflect0[A, B])

    @inline def reify0[A](m: A :#: List[A]): List[A] = m(List(_))

    @inline def reify1[A, R](m: A :#: List[A] :#: R): List[A] :#: R = m(a => pure(List(a)))

    @inline def reify[A, R](m: (A :#: List[A]) :#: R): List[A] :#: R = m.map(reify0)

    @inline def reflect[A, B, R](m: List[A]): A :#: List[B] :#: List[R] =
      shift1((k3: A => List[B] :#: List[R]) =>
        shift0((k1: List[B] => List[R]) =>
          m.flatMap(a => k3(a)(k1))
        )
      )

    @inline def reflect2[A, B, R](m: List[A]) =
      shift1((k3: A => List[R] :#: R) =>
        shift0((k1: List[R] => R) =>
          k1(m.flatMap(a => List(k3(a)(k1))))
        )
      )

    @inline def reflect3[A, B, R](m: List[A]) =
      reflect0[A, R](m).lift[B]

    @inline def reflect0[A, B](m: List[A]): A :#: List[B] = shift0((k: A => List[B]) => m.flatMap(k))

    @inline def reflect1[A, B, R](m: List[A]): Cont[A, List[B], List[B] :#: R] =
      shift0((k1: A => List[B]) => shift0((k2: List[B] => R) => k2(m.flatMap(k1))))

    @inline def reflect10[A, B, R](m: List[A]) =
      shift0((k: List[B] => List[B] :#: R) =>
        shift0((k1: A => List[B]) =>
          //shift0((k2: List[B] => R) =>
          k(m.flatMap(k1)))
        //)
      )


  }
}
