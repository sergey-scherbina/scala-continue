package cont

import org.junit.Assert._
import org.junit.Test

import scala.util.control.TailCalls.TailRec

class TestCont {

  @Test def t1(): Unit = {

    val fib = loop(for {
      (x, y) <- channel[(BigInt, BigInt), LazyList[BigInt]]
      _ <- put(x)
    } yield (y, x + y))(0, 1)

    assertEquals(
      List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34),
      fib.take(10).toList
    )
  }

  @Test def t2(): Unit = {

    val example1 = reset(for {
      _ <- delay(println("Hello,"))
      _ <- delay(println("World!"))
      _ <- delay(println("Goodbye!"))
    } yield ())

    val example2 = reset(for {
      _ <- delay(println("1"))
      _ <- delay(println("2"))
      _ <- delay(println("3"))
      _ <- delay(println("4"))
    } yield ())

    par(par(par(example1, example2)))
  }

  @Test def t3(): Unit = {
    enum Console[A] {

      case Input() extends Console[String]

      case Output(s: String) extends Console[Unit]

    }

    import Console._

    val p = reset(for {
      _ <- raise(Output("Enter a:"))
      a <- raise(Input())
      _ <- raise(Output("Enter b:"))
      b <- raise(Input())
      _ <- raise(Output(a ++ b))
    } yield ())

    val console = handler_ {
      case ((i: List[String], o: List[String]), Input()) =>
        ((i.tail, o), _ (i.head))
      case ((i: List[String], o: List[String]), Output(out)) =>
        ((i, out :: o), _ (println(out)))
    }

    assertEquals(((List(), List("Hello,World", "Enter b:", "Enter a:")), ()),
      console(p)((List("Hello,", "World"), List())))

  }

}