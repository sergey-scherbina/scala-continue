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

  test("search tree") {
    // http://okmij.org/ftp/continuations/Searches.hs
    //
    // data SearchTree a = Leaf a | Node [() -> SearchTree a]
    sealed trait SearchTree[A]
    case class Leaf[A](a: A) extends SearchTree[A]
    case class Node[A](b: Stream[Unit => SearchTree[A]]) extends SearchTree[A]
    /*
    bfs :: SearchTree a -> [a]
    bfs tree = loop [\() -> tree]
    where
      loop []    = []
      loop (h:t) = case h () of
               Leaf x -> x : loop t
               Node b -> loop (t ++ b)
     */
    def bfs[A](tree: SearchTree[A]): Stream[A] = {
      def loop(node: Stream[Unit => SearchTree[A]]): Stream[A] =
        node match {
          case Stream() => Stream()
          case h #:: t => h() match {
            case Leaf(x) => x #:: loop(t)
            case Node(b) => loop(b #::: t)
          }
        }

      loop(Stream(_ => tree))
    }

    /*
    -- Non-deterministic choice from a _finite_ list
    -- This is the only primitive. Everything else is implemented in terms
    -- of choose
    choose :: [a] -> Cont (SearchTree w) a
    choose lst = shift (\k -> return $ Node (map (\x () -> k x) lst))

    -- Failing computation
    failure :: Cont (SearchTree w) a
    failure = choose []

    -- How to run non-deterministic computation
    reify :: Cont (SearchTree a) a -> SearchTree a
    reify m = runC (fmap Leaf m)
     */
    def choose[A, B](as: Stream[A]): Cont[SearchTree[B], SearchTree[B], A] =
      shift((k: A => SearchTree[B]) => Node(as.map(x => (_: Unit) => k(x))))

    def failure[A, B](): Cont[SearchTree[B], SearchTree[B], A] =
      choose[A, B](Stream())

    def reify[A](m: Cont[SearchTree[A], SearchTree[A], A]): SearchTree[A] =
      reset(identity(m).map(Leaf(_)))

    /*
    ex1 = reify $ do
      x <- choose [1..10]
      y <- choose [1..10]
      z <- choose [1..10]
      if x*x + y*y == z*z then return (x,y,z) else failure
     */
    def ex1(s: Stream[Int]): SearchTree[(Int, Int, Int)] = reify(
      choose(s) >>= (x => choose(s) >>= (y => choose(s) >>= (z =>
        if (x * x + y * y == z * z) pure((x, y, z)) else failure()
        )))
    )

    bfs(ex1(Stream.from(1).take(10)))
      .foreach(println)

    println()

    def triple(x: Int, y: Int, z: Int): Cont[
      SearchTree[(Int, Int, Int)],
      SearchTree[(Int, Int, Int)],
      (Int, Int, Int)
    ] = if (x * x + y * y == z * z)
      pure((x, y, z)) else failure()

    def ex2(s: Stream[Int]): SearchTree[(Int, Int, Int)] =
      reify(for {
        x <- choose(s)
        y <- choose(s)
        z <- choose(s)
        r <- triple(x, y, z)
      } yield r)

    bfs(ex2(Stream.from(1).take(100))).
      foreach(println)

  }

  test("prefixes2") {
    //https://brics.dk/RS/05/11/BRICS-RS-05-11.pdf
    // "An Operational Foundation for Delimited Continuations in the CPS Hierarchy"

    def firstPrefix1[A](p: A => Boolean, xs: List[A]): List[A] = {
      def visit(xs: List[A], k: List[A] => List[A]): List[A] =
        xs match {
          case List() => List()
          case x :: xs => {
            val k1 = (vs: List[A]) => k(x :: vs)
            if (p(x)) k1(Nil) else visit(xs, k1)
          }
        }

      visit(xs, vs => vs)
    }

    println(firstPrefix1((_: Int) > 2, List(0, 3, 1, 4, 2, 5)))

    def allPrefixes1[A](p: A => Boolean, xs: List[A]): List[List[A]] = {
      def visit(xs: List[A], k: List[A] => List[A]): List[List[A]] =
        xs match {
          case List() => List()
          case x :: xs => {
            val k1 = (vs: List[A]) => k(x :: vs)
            if (p(x)) k1(Nil) :: visit(xs, k1) else visit(xs, k1)
          }
        }

      visit(xs, vs => vs)
    }

    println(allPrefixes1((_: Int) > 2, List(0, 3, 1, 4, 2, 5)))

    def firstPrefix0[A](p: A => Boolean, xs: List[A]): List[A] = {
      def visit(xs: List[A]): Cont[List[A], List[A], List[A]] =
        xs match {
          case List() => shift((k: List[A] => List[A]) => List[A]())
          case x :: xs =>
            for {y <- if (p(x))
              pure[List[A], List[A]](Nil)
            else visit(xs)} yield x :: y
        }

      reset(visit(xs))
    }

    println(firstPrefix0((_: Int) > 2, List(0, 3, 1, 4, 2, 5)))

    def allPrefixes0[A](p: A => Boolean, xs: List[A]): List[List[A]] = {
      def visit(xs: List[A]): Cont[List[List[A]], List[A], List[A]] =
        xs match {
          case List() => shift((k: List[A] => List[A]) => List[List[A]]())
          case x :: xs =>
            for {y <- if (p(x))
              shift((k: List[A] => List[A]) => k(Nil) ::
                reset(for (y <- visit(xs)) yield k(y)))
            else visit(xs)} yield x :: y
        }

      reset(visit(xs))
    }

    println(allPrefixes0((_: Int) > 2, List(0, 3, 1, 4, 2, 5)))

    def firstPrefix2[A](p: A => Boolean, xs: List[A]): List[A] = {
      def visit(xs: List[A],
                k1: (List[A], List[A] => List[A]) => List[A],
                k2: List[A] => List[A]): List[A] =
        xs match {
          case List() => k2(List())
          case x :: xs => {
            val k1_ =
              (vs: List[A], k2_ : List[A] => List[A]) => k1(x :: vs, k2_)
            if (p(x)) k1_(Nil, k2) else visit(xs, k1_, k2)
          }
        }

      visit(xs, (vs, k2) => k2(vs), vs => vs)
    }

    println(firstPrefix2((_: Int) > 2, List(0, 3, 1, 4, 2, 5)))

    def allPrefixes2[A](p: A => Boolean, xs: List[A]): List[List[A]] = {
      def visit(xs: List[A],
                k1: (List[A], List[A] => List[List[A]]) => List[List[A]],
                k2: List[List[A]] => List[List[A]]): List[List[A]] =
        xs match {
          case List() => k2(List())
          case x :: xs => {
            val k1_ =
              (vs: List[A], k2_ : List[A] => List[List[A]]) => k1(x :: vs, k2_)
            if (p(x))
              k1_(Nil, vs => visit(xs, k1_, vss => k2(vs :: vss)))
            else visit(xs, k1_, k2)
          }
        }

      visit(xs, (vs, k2) => k2(vs), vs => vs)
    }

    println(allPrefixes2((_: Int) > 2, List(0, 3, 1, 4, 2, 5)))

  }

  test("prefixes3") {
    // http://www.ii.uni.wroc.pl/~mabi/papers/biernacka-al-ppdp11.pdf
    {
      def walk[A, B, C](xs: List[A]): (List[A] => (B => C) => C) => (List[B] => C) => C =
        k1 => k2 => xs match {
          case Nil => k2(List())
          case (x :: xs) => k1(List(x))(vs =>
            walk(xs)(vs => k1(x :: vs))(vss => k2(vs :: vss)))
        }

      def prefixes[A](xs: List[A]): List[List[A]] = walk(xs)(
        (vs: List[A]) => (k2: List[A] => List[List[A]]) => k2(vs)
      )(vss => vss)

      println(prefixes(List(1, 2, 3, 4)))
    }
    {
      def shift1[A, B, C, D](f: (A => B) => Cont[C, D, D]): Cont[C, B, A] =
        shift((k: A => B) => reset(f(k)))

      def reset1[A, B, C](e: Cont[A, B, B]): Cont[C, C, A] = pure(reset(e))

      def walk[A](xs: List[A]): Cont[List[List[A]], List[A], List[A]] = xs match {
        case Nil => shift1(_ => pure(List[List[A]]()))
        case x :: xs => shift1(k => for (z <- reset1(
          for (y <- walk(xs)) yield k(x :: y)))
          yield k(List(x)) :: z)
      }

      def prefixes[A](xs: List[A]): List[List[A]] = reset(walk(xs))

      println(prefixes(List(1, 2, 3, 4)))
    }

    /*
        {
          def fail[A, B]: Unit => A => (Unit => B) => B
          = _ => k1 => k2 => k2()

          def amb[A, B]: (Unit => A => (Unit => B) => B) =>
            (Unit => A => (Unit => B) => B) =>
              (A => (Unit => B) => B)
          = c1 => c2 => k1 => k2 => c1()(k1)(_ => c2()(k1)(k2))

          def walk[A, B]: List[A] => (List[A] => (Unit => B) => B) => (Unit => B) => B = {
            case Nil => fail(())
            case (x :: xs) =>
              amb(_ =>
                (k1: List[A] => (Unit => List[A]) => List[A]) => k1(x :: List())
              )(_ =>
                (k1: List[A] => (Unit => List[A]) => List[A]) => walk(xs)(vs => k1(x :: vs)))
          }

          def emit[A, B, C, D]: A
            => (Unit => (Unit => (List[A] => B) => C) => D)
            => (Unit => (List[A] => B) => C) => D
          = v => k1 => k2 =>
            k1()(_ => k3 => k2()(u => k3(v :: u)))

          def collect[A, B, C, D, E, F, G]: (Unit =>
            (A => (Unit => (List[A] => B) => C) => (List[A] => B) => C)
              => (Unit => (List[D] => E) => E) => (F => F) => List[List[A]]) => List[List[A]]
          = c => c()(v => emit(v)(u => k2 => k2(u)))(_ => k3 => k3(Nil))(vs => vs)

          def prefixes[A]: List[A] => List[List[A]] = xs => collect(_ => walk(xs))

        }
    */

    /*
        {
          type Ans1 = Unit
          type Ans2 = List[List[Int]]
          type Cont1[A] = Cont[Ans1, Ans1, A]
          type Cont2[A] = Cont[Cont1[Ans2], Cont1[Ans2], A]

          def shift_1[A](k: (A => Ans1) => Ans1): Cont1[A] = shift(k)

          def reset_1[A](e: Unit => Cont1[A]): Ans1 = reset(e())

          def shift_2[A](k: Cont2[A]) = k

          def reset_2[A](e: Unit => Cont[Ans2, Ans2, A]): Ans2 = reset(e())

          def fail[A](): Cont[Ans1, Ans1, A] = shift_1(k => ())

          def amb[A](c1: Unit => A, c2: Unit => A): Cont[Ans1, Ans1, A] =
            shift_1 { k =>
              k(c1());
              k(c2())
            }

          def emit(v: Int): Cont[Ans2, Ans2, Ans1] = shift_2((k: Ans1 => Ans2) => v :: k())

          def collect(c: Unit => Int) = reset_2 { _ =>
            val _ = reset_1(_ => emit(c()))
            pure(List[Int]())
            )
          }
        }
    */


  }
}
