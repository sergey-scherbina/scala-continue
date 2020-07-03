package cont

object TestConsole extends App {

  enum Console1[A] {

    case Input1() extends Console1[String]

    case Output1(s: String) extends Console1[Unit]

  }

  enum Console2[A] {

    case Input2() extends Console2[String]

    case Output2(s: String) extends Console2[Unit]

  }

  import Console1._
  import Console2._

  lazy val console1 = handle {
    case Input1() => _ (io.StdIn.readLine())
    case Output1(out) => _ (println(out))
  }

  lazy val console2 = handle {
    case Input2() => _ (io.StdIn.readLine().reverse)
    case Output2(out) => _ (println(out.reverse))
  }

  lazy val console = console1 compose console2

  val e = reset(for {
    _ <- raise(Output1("Enter a:"))
    a <- raise(Input1())
    _ <- raise(Output2("Enter b:"))
    b <- raise(Input2())
    _ <- raise(Output1(a ++ b))
  } yield (a, b))

  console(e)

}
