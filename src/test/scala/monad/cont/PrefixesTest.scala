package monad.cont

import org.scalatest.FunSuite

class PrefixesTest extends FunSuite {

  test("append") {
    def append[A](lst: List[A]): Cont[List[A] => List[A], List[A], List[A]] =
      lst match {
        case List() => shift(identity)
        case a :: rest => append(rest).map(a :: _)
      }

    val app123 =
      reset(append(List(1, 2, 3)))

    println(app123(List(5, 6, 7)))
  }

  test("prefixes") {

    def w[A](lst: List[A]): Cont[List[List[A]], List[A], List[A]] =
      lst match {
        case List() => shift(_ => List())
        case x :: xs => shift((k: List[A] => List[A]) =>
          k(List()) :: reset(w(xs).map(k))).map(x :: _)
      }

    def prefixes[A](xs: List[A]) =
      reset(w(xs))

    println(prefixes(List(1, 2, 3)))

  }

  test("partition") {
    def partition[T: Ordering](a: T, l: List[T]) = {
      val ord = Ordering[T]
      import ord._

      type C[A] = Cont[A, A, A]

      def mapC[A, B](c: Cont[A, A, A])(f: A => B): Cont[A, A, B] =
        identity(c).map(f)

      def part(l: List[T]): C[C[List[T]]] = l match {
        case List() => pure(pure(List()))
        case h :: t => if (h > a)
          mapC(part(t))(mapC(_)(h :: _))
        else if (h == a)
          shift((f: C[List[T]] => C[List[T]]) =>
            mapC(reset(mapC(part(t))(f)))(h :: _))
        else shift((f: C[List[T]] => C[List[T]]) =>
          shift((g: List[T] => List[T]) =>
            h :: reset(mapC(reset(mapC(part(t))(f)))(g))
          ))
      }

      reset(reset(part(l)))
    }

    println(partition(3, List(4, 1, 3, 5, 2, 3)))
  }

}
