package cont

import cont.Cont1._
import org.scalatest.FunSuite

class ReflectTest extends FunSuite {
  test("union") {

    trait in[A, B]
    object in {
      implicit def inLeft[A, B]: (A in (A Either B)) = null
      implicit def inRight[A, B]: (A in (B Either A)) = null
      implicit def inLeftRec[A, B, C](implicit r: A in B): (A in (B Either C)) = null
      implicit def inRightRec[A, B, C](implicit r: A in C): (A in (B Either C)) = null
    }

    type |[A, B] = Either[A, B]

    def test1(implicit q: (Boolean in (String | Int | Boolean | Float))) = "test1"

    def test2(implicit q: (Char in (String | Int | Boolean | Float))) = "test2"

    println(test1)
    //println(test2)

  }

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

  test("test1") {

    // 1 + ⟨10 + S0k.100 + k (k 0)⟩

    def f1() = 1 + reset0(fmap(
      shift0((k: Int => Int) =>
        100 + k(k(0))))((x: Int) =>
      x + 10))

    println(f1())
    assert(f1() === 121)

    /*
    ⟨1 + ⟨10 × S0k1.S0k2.k1 (k2 0)⟩⟩
     */

    def f2() = {
      reset0(fmap(reset0(fmap(shift0((k1: Int => Int) =>
        shift0((k2: Int => Int) => k1(k2(0)))))(_ * 10)))(_ + 1))
    }

    println(f2())
    assert(f2() === 10)

    def f3() = {
      fmap(fmap(shift0((k1: Int => Int) => shift0((k2: Int => Int) => k1(k2(0)))))(_ * 10))(_ + 1)
    }

    def f4() = {
      shift0((k1: Int => Int) => shift0((k2: Int => Int) => k1(k2(0)))).map(_ * 10).map(_ + 1)
    }

    def f5() = {
      shift0((k1: Int => Int) => shift0((k2: Int => Int) => k1(k2(0)))).map(_ * 10).map(_ + 1)
    }

  }

}
