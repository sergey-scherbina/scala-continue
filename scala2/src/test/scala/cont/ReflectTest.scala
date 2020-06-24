package cont

import cont.Cont1._
import org.scalatest.FunSuite

class ReflectTest extends FunSuite {
  test("union1") {
    trait in[A, B] {
      def inj(a: A): B
    }
    object in {
      implicit def inSelf[A]: (A in A) = (a: A) => a
      implicit def inHead[A, B]: (A in (A Either B)) = (a: A) => Left(a)
      implicit def inRight[A, B, C](implicit r: (A in C)): (A in (B Either C)) =
        (a: A) => Right(r.inj(a))
      implicit def inLeft[A, B, C](implicit r: (A in B)): (A in (B Either C)) =
        (a: A) => Left(r.inj(a))
    }

    def inj[B] = new {
      def apply[A](a: A)(implicit r: (A in B)): B = r.inj(a)
    }

    def handle[A, B, C](a: (A Either B))(f: A => C): Option[B] =
      a.fold(a => Function.const(None)(f(a)), Some(_))

    type :|:[A, B] = Either[A, B]
    type All = String :|: Int :|: Boolean :|: Double

    def handleAll[A](a: A)(implicit r: (A in All)) =
      handle(inj(a))(x => println(x + " : String"))
        .flatMap(handle(_)(x => println(x + " : Int")))
        .flatMap(handle(_)(x => println(x + " : Boolean")))
        .map(x => println(x + " : Double"))

    handleAll("a")
    handleAll(1)
    handleAll(true)
    handleAll(1.0)
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
