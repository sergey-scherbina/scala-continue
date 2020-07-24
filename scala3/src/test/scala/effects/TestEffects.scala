package effects

import cont._
import cont.{given _}

import org.junit.Test

class TestEffects {

  @Test def t1(): Unit = {

    enum In[A] {

      case Input() extends In[String]

    }

    enum Out[A] {

      case Output(s: String) extends Out[Unit]

    }

    import In._, Out._

    def input[E[_], A] = effect(Input())[E, A]()

    def output[E[_], A](s: String) = effect(Output(s))[E, A]()

    val e1 = reset(for {
      _ <- output("")
      a <- input
      _ <- output(a)
      b <- input
      _ <- output(b)
    } yield ())

    println(e1: Ef[In :||: Out, Unit])

    (e1: Ef[Out :||: In, Unit]) match {
      case Ef.Req(Input(), k) => println("In")
      case Ef.Req(Output(o), k) => println("Out")
    }

    val in = handler {
      case In.Input() => _ ("*")
    }

    val out = handler {
      case Out.Output(s) => _ (s)
    }

  }

}
