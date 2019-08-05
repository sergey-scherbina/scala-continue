package monad.cont

import org.scalatest.FunSuite

import scala.collection.immutable.Stream.Empty

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
      def loop(node: Stream[Unit => SearchTree[A]]): Stream[A] = node match {
        case Stream() => Stream()
        case h #:: t => h() match {
          case Leaf(x) => x #:: loop(t)
          case Node(b) => loop(t ++ b)
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
    def ex1(): SearchTree[(Int, Int, Int)] = reify(
      choose(Stream.from(1).take(10)) >>= (x =>
        choose(Stream.from(1).take(10)) >>= (y =>
          choose(Stream.from(1).take(10)) >>= (z =>
            if (x * x + y * y == z * z)
              pure((x, y, z)) else failure())))
    )

    bfs(ex1()).foreach(println)

    println()

    def tripple(x: Int, y: Int, z: Int): Cont[
      SearchTree[(Int, Int, Int)],
      SearchTree[(Int, Int, Int)],
      (Int, Int, Int)
    ] = if (x * x + y * y == z * z)
      pure((x, y, z)) else failure()

    def ex2(): SearchTree[(Int, Int, Int)] = reify(for {
      x <- choose(Stream.from(1).take(10))
      y <- choose(Stream.from(1).take(10))
      z <- choose(Stream.from(1).take(10))
      r <- tripple(x, y, z)
    } yield r)

    bfs(ex2()).foreach(println)

  }
}
