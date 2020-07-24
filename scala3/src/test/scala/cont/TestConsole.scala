package cont

object TestConsole extends App {

  enum Console[A] {

    case Input() extends Console[String]

    case Output(s: String) extends Console[Unit]

  }

  enum Console1[A] {

    case Input1() extends Console1[String]

    case Output1(s: String) extends Console1[Unit]

  }

  import Console._
  import Console1._

  val p = process_((w: Int) => for {
    _ <- raise(Output(s"$w"))
    _ <- raise(Output("Enter a:"))
    a <- raise(Input())
    _ <- raise(Output1("Enter b:"))
    b <- raise(Input1())
    _ <- raise(Output(a ++ b))
    _ <- raise(Output1("\nExit?[y/n]:"))
    x <- raise(Input1())
    _ <- if (x != "y") continue() else abort()
  } yield w + 1)

  val console0 = handler {
    case Input() => _ (io.StdIn.readLine())
    case Output(out) => _ (println(out))
  }

  val console1 = handler {
    case Input1() => _ (io.StdIn.readLine())
    case Output1(out) => _ (println(out))
  }

  val console = console0 compose console1

  console(p(1))

}
