package cont

import cont.Cont._
import org.scalatest.FunSuite

class PraxisTest extends FunSuite {

  test("append") {

    def append[A](xs: List[A]): List[A] => List[A] = {

      def walk: List[A] => Cont[List[A], List[A], List[A] => List[A]] = {
        case List() => shift0(identity)
        case h :: t => walk(t).map(h :: _)
      }

      reset0(walk(xs))
    }

    assert {
      append(List(1, 2, 3, 4))(List(5, 6, 7, 8)) === List(1, 2, 3, 4, 5, 6, 7, 8)
    }
  }

  test("prefixes") {

    def prefixes[A](xs: List[A]): List[List[A]] = {
      
      def walk: List[A] => Cont[List[A], List[A], List[List[A]]] = {
        case List() => shift0(_ => List())
        case x :: xs => shift0(k => k(List(x)) :: reset0(
          for (vs <- walk(xs)) yield k(x :: vs)))
      }

      reset0(walk(xs))
    }

    assert {
      prefixes(List(1, 2, 3, 4)) === List(List(1), List(1, 2), List(1, 2, 3), List(1, 2, 3, 4))
    }
  }

}
