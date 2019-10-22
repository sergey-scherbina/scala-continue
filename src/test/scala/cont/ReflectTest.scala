package cont

import cont.Cont1._
import org.scalatest.FunSuite

class ReflectTest extends FunSuite {
  test("union1") {

    sealed trait in[A, B] {
      type Tail
      def proj(b: B): Option[A]
      def inj(a: A): B
      def decomp(b: B): Either[Tail, A] = proj(b)
        .fold(Left(b.asInstanceOf[Tail]): Either[Tail, A])(Right(_))
    }

    object in {
      implicit def emptyIn[A]: (Unit in A) = new (Unit in A) {
        override type Tail = Unit
        override def proj(b: A): Option[Unit] = None
        override def inj(a: Unit): A = ???
      }
      implicit def inSelf[A]: (A in A) = new (A in A) {
        override type Tail = Unit
        override def proj(b: A): Option[A] = Some(b)
        override def inj(a: A): A = a
      }
      implicit def inLeft[A, B]: (A in (A Either B)) = new (A in (A Either B)) {
        override type Tail = B
        override def proj(b: Either[A, B]): Option[A] = b.fold(Some(_), _ => None)
        override def inj(a: A): Either[A, B] = Left(a)
      }
      implicit def inRight[A, B]: (A in (B Either A)) = new (A in (B Either A)) {
        override type Tail = B
        override def proj(b: Either[B, A]): Option[A] = b.fold(_ => None, Some(_))
        override def inj(a: A): Either[B, A] = Right(a)
      }
      implicit def inLeftRec[A, B, C](implicit r: A in B): (A in (B Either C)) = new (A in (B Either C)) {
        override type Tail = Either[r.Tail, C]
        override def proj(b: Either[B, C]): Option[A] = b.fold(r.proj, _ => None)
        override def inj(a: A): Either[B, C] = Left(r.inj(a))
      }
      implicit def inRightRec[A, B, C](implicit r: A in C): (A in (B Either C)) = new (A in (B Either C)) {
        override type Tail = Either[B, r.Tail]
        override def proj(b: Either[B, C]): Option[A] = b.fold(_ => None, r.proj)
        override def inj(a: A): Either[B, C] = Right(r.inj(a))
      }
    }

    type In[B] = {type is[A] = A in B}
    object In {
      def apply[B] = new {
        def apply[A: In[B]#is]: (A in B) = implicitly[A in B]
      }
    }

    type |[A, B] = Either[A, B]
    type All = String | Int | Boolean | Float

    def test1[T: In[All]#is](a: T): T = a

    def handle[B, A: In[B]#is](b: B)(f: A => Unit): Option[(A in B)#Tail] =
      In[B][A].decomp(b).fold(Some(_), a => {
        f(a);
        Option.empty[(A in B)#Tail]
      })

//    def handleAll(a: All) =
//      handle(a)(println(_: String))
//        .flatMap(handle(_)(println(_: Int)))
//        .flatMap(handle(_)(println(_: Boolean)))
//        .flatMap(handle(_)(println(_: Float)))

    println(test1(true))

    println(In[All][Boolean].inj(true))
    println(In[All][Boolean].decomp(In[All][String].inj("hello")))
    println(In[All][Boolean].decomp(In[All][Boolean].inj(true)))

    //println(In[All][Char].to('a'))
    // println(test1('a'))
    // println(In[All][Char].decomp(In[All][Boolean].inj(true)))
    // println(In[All][Boolean].decomp(In[All][Char].inj('a')))

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
