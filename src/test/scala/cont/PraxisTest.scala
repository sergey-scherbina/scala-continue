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

  test("pythagorian triples") {

    def triple(x: Int, y: Int, z: Int): Unit :#: Unit :#: List[(Int, Int, Int)] =
      if (x * x + y * y == z * z) emit0((x, y, z)).lift else fail1

    def pyth(n: Int): List[(Int, Int, Int)] = collect0(reset1(for {
      x <- (1 to n).toList.reflect0.lift
      y <- (x to n).toList.reflect0.lift
      z <- (y to n).toList.reflect0.lift
      _ <- triple(x, y, z)
    } yield ()))

    println(pyth(10))

    def tripleS(x: Int, y: Int, z: Int): Unit :#: Unit :#: Stream[(Int, Int, Int)] =
      if (x * x + y * y == z * z) emitS0((x, y, z)).lift else fail1

    def pythS(n: Int): Stream[(Int, Int, Int)] = collectS0(reset1(for {
      x <- (1 to n).toStream.reflect0.lift
      y <- (x to n).toStream.reflect0.lift
      z <- (y to n).toStream.reflect0.lift
      _ <- tripleS(x, y, z)
    } yield ()))

    println(pythS(10))
  }

  test("state") {

    //State s r a ~ s -> (a -> s -> r) -> r

    type State[A, S, R] = A :#: (S => R)

    def get[S, R]: S :#: (S => R) = shift0(k => s => k(s)(s))

    def set[S, R](s1: S): Unit :#: (S => R) = shift0(k => _ => k()(s1))

    def tick[R]: Unit :#: (Int => R) = for (
      n <- get[Int, R]; _ <- set[Int, R](n + 1)) yield ()

    def runState[S, R](e: R :#: (S => R)): S => R = e(k => _ => k)

    def mkList(n: Int): List[Int] = {
      def aux(n: Int): List[Int] :#: (Int => List[Int]) =
        if (n > 0)
          for (_ <- tick[List[Int]];
               x <- get[Int, List[Int]];
               xs <- aux(n - 1))
            yield x :: xs
        else pure(List())

      runState(aux(n))(0)
    }

    assert(mkList(10) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  }

}
