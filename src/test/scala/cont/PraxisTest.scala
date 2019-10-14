package cont

import cont.Cont1._
import org.scalatest.FunSuite

import scala.annotation.tailrec

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

  test("partition") {

    def partition(a: Int, l: List[Int]): Unit :#: List[Int] :#: List[Int] = l match {
      case List() => pure(())
      case (h :: t) =>
        if (a < h) for {
          _ <- emit1(h)
          _ <- partition(a, t)
        } yield () else for {
          _ <- emit0(h).lift
          _ <- partition(a, t)
        } yield ()
    }

    println(reset0(collect1(partition(3, List(4, 1, 3, 5, 2, 3)))))

  }

  test("prefixes evolution") {
    def prefixes[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A], p: List[A], r: List[List[A]]): List[List[A]] =
        xs match {
          case List() => r
          case y :: ys =>
            val p1 = p :+ y
            visit(ys, p1, r :+ p1)
        }

      visit(xs, List(), List())
    }

    println(prefixes(List(1, 2, 3, 4)))

    def prefixes1[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A], k: List[A] => List[A]): List[List[A]] =
        xs match {
          case List() => List()
          case y :: ys =>
            val k1 = (vs: List[A]) => k(y :: vs)
            k1(Nil) :: visit(ys, k1)
        }

      visit(xs, x => x)
    }

    println(prefixes1(List(1, 2, 3, 4)))

    def prefixes2[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A])(k: List[A] => List[A]): List[List[A]] =
        xs match {
          case List() => List()
          case y :: ys =>
            val k1 = (vs: List[A]) => k(y :: vs)
            k1(Nil) :: visit(ys)(k1)
        }

      visit(xs)(x => x)
    }

    println(prefixes2(List(1, 2, 3, 4)))

    def prefixes3[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A]): (List[A] => List[A]) => List[List[A]] =
        k => xs match {
          case List() => List()
          case y :: ys =>
            val k1 = (vs: List[A]) => k(y :: vs)
            k1(Nil) :: visit(ys)(k1)
        }

      visit(xs)(x => x)
    }

    println(prefixes3(List(1, 2, 3, 4)))

    def prefixes4[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A]): Cont[List[A], List[A], List[List[A]]] =
        xs match {
          case List() => ((k: List[A] => List[A]) => List())
          case y :: ys => ((k: List[A] => List[A]) => {
            val k1 = (vs: List[A]) => k(y :: vs)
            k1(Nil) :: visit(ys)(k1)
          })
        }

      visit(xs)(x => x)
    }

    println(prefixes4(List(1, 2, 3, 4)))

    def prefixes5[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A]): Cont[List[A], List[A], List[List[A]]] =
        xs match {
          case List() => shift0((k: List[A] => List[A]) => List())
          case y :: ys => shift0((k: List[A] => List[A]) => {
            val k1 = (vs: List[A]) => k(y :: vs);
            k1(Nil) :: visit(ys)(k1)
          })
        }

      reset0(visit(xs))
    }

    println(prefixes5(List(1, 2, 3, 4)))

    def prefixes6[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A]): Cont[List[A], List[A], List[List[A]]] =
        xs match {
          case List() => shift0((k: List[A] => List[A]) => List())
          case y :: ys => k1 =>
            ((k: List[A] => List[A]) => k(Nil) :: visit(ys)(k)
              ) ((vs: List[A]) => k1(y :: vs))
        }

      reset0(visit(xs))
    }

    println(prefixes6(List(1, 2, 3, 4)))

    def prefixesM[A](xs: List[A]): List[List[A]] = {
      def visit(xs: List[A]): Cont[List[A], List[A], List[List[A]]] =
        xs match {
          case List() => shift0((k: List[A] => List[A]) => List())
          case y :: ys =>
            shift0((k: List[A] => List[A]) => k(Nil) :: visit(ys)(k))
              .map((vs: List[A]) => (y :: vs))
        }

      reset0(visit(xs))
    }

    println(prefixesM(List(1, 2, 3, 4)))

    def prefixesC[A](xs: List[A]) = {
      def visit[A](lst: List[A]): Cont[List[A], List[A], List[List[A]]] =
        lst match {
          case List() => shift0(_ => List())
          case x :: xs => for {
            vs <- shift0((k: List[A] => List[A]) =>
              k(Nil) :: visit(xs)(k))
          } yield x :: vs
        }

      reset0(visit(xs))
    }

    println(prefixesC(List(1, 2, 3, 4)))

  }


  test("state") {

    def mkList(n: Int): List[Int] = {
      def aux(n: Int): List[Int] :#: (Int => List[Int]) =
        n match {
          case 0 => pure(List())
          case _ =>
            for (_ <- increase0;
                 x <- get0;
                 xs <- aux(n - 1))
              yield x :: xs
        }

      runState(aux(n))(0)
    }

    assert(mkList(10) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

    def mkList1(n: Int): List[Int] = n match {
      case 0 => List()
      case m => m :: mkList1(m - 1)
    }

    println(mkList1(10))

    def mkList2(n: Int, s: Int = 1): List[Int] = n match {
      case 0 => List()
      case _ => s :: mkList2(n - 1, s + 1)
    }

    println(mkList2(10))

    def mkList0(n: Int): List[Int] = {

      def aux(n: Int): List[Int] :#: (Int => List[Int]) =
        n match {
          case 0 => pure(List())
          case _ => for {
            x <- increase0
            xs <- aux(n - 1)
          } yield x :: xs
        }

      runState(aux(n))(1)
    }

    println(mkList0(10))

  }

  test("nondet") {

    def tripl0(a: Int, b: Int, c: Int): Unit :#: List[(Int, Int, Int)] =
      if (a * a + b * b == c * c) emit0((a, b, c)) else pure(())

    def pyth0(n: Int): List[(Int, Int, Int)] = collect0(for {
      x <- (1 to n).toList.reflect0
      y <- (x to n).toList.reflect0
      z <- (y to n).toList.reflect0
      _ <- tripl0(x, y, z)
    } yield ())

    println(pyth0(10))
  }

  test("same fringe") {
    sealed trait Gen[A]
    case class End[A]() extends Gen[A]
    case class Next[A](a: A, n: Unit => Gen[A]) extends Gen[A]

    @tailrec
    def same[A](g1: Gen[A], g2: Gen[A]): Boolean = (g1, g2) match {
      case (End(), End()) => true
      case (Next(a1, n1), Next(a2, n2))
        if (a1 == a2) => same(n1(), n2())
      case _ => false
    }

    sealed trait Tree[A]
    case class Leaf[A](a: A) extends Tree[A]
    case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

    def gen0[A](t: Tree[A]): List[A] = {
      def visit(t: Tree[A]): List[A] = t match {
        case Leaf(a) => List(a)
        case Node(t1, t2) => visit(t1) ++ visit(t2)
      }

      visit(t)
    }

    def gen1[A](t: Tree[A]): Gen[A] = {
      def visit(t: Tree[A])(g: Unit => Gen[A]): Gen[A] = t match {
        case Leaf(a) => Next(a, g)
        case Node(t1, t2) => visit(t1)(_ => visit(t2)(g))
      }

      visit(t)(_ => End())
    }

    def gen2[A](t: Tree[A]): Gen[A] = {
      def visit(t: Tree[A]): Unit :#: Gen[A] = t match {
        case Leaf(a) => shift0(Next(a, _))
        case Node(t1, t2) => visit(t1) >> visit(t2)
      }

      reset0(visit(t) >>> End[A]())
    }

    val t1 = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    val t2 = Node(Leaf(1), Node(Leaf(2), Leaf(3)))

    println(same(gen1(t1), gen1(t2)))
    println(same(gen2(t1), gen2(t2)))

  }

}
