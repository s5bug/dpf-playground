package tf.bug.dpfplayground.component

import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.{Signal, SignallingRef}
import fs2.dom.{HtmlElement, MouseEvent}

object Tabs {

  enum Tab extends java.lang.Enum[Tab] {
    case Generate
    case Eval
    case EvalSeq
  }
  given hashTab: cats.kernel.Hash[Tab] with {
    override def hash(x: Tab): Int = x.hashCode()
    override def eqv(x: Tab, y: Tab): Boolean = x == y
  }

  def apply(): Resource[IO, HtmlElement[IO]] = {
    (Resource.eval(SignallingRef[IO, Tab](Tab.Generate)), Generate(), Eval(), EvalSeq()).flatMapN { (selected, gen, eval, seq) =>
      div(
        navTag(
          children[Tab] { tt =>
            button(tt.toString, disabled <-- selected.map(_ == tt), onClick --> { _.foreach(_ => selected.set(tt)) })
          } <-- Signal.constant(Tab.values.toList),
        ),
        selected.map {
          case Tab.Generate => gen
          case Tab.Eval => eval
          case Tab.EvalSeq => seq
        }.map(c => Resource.pure[IO, HtmlElement[IO]](c))
      )
    }
  }

}
