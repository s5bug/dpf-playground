package tf.bug.dpfplayground.component

import calico.html.io.{*, given}
import cats.effect.*
import fs2.dom.HtmlElement

object EvalSeq {

  def apply(): Resource[IO, HtmlElement[IO]] = {
    div(
      h1("EvalSeq")
    )
  }
  
}
