package tf.bug.dpfplayground.component

import calico.html.io.{given, *}
import cats.effect.*
import fs2.dom.HtmlElement

object Generate {

  def apply(): Resource[IO, HtmlElement[IO]] = {
    div(
      h1("Generate"),
      div(
        idAttr := "generate-columns",
        div(
          idAttr := "generate-p0",
          h2("Party 0"),
        ),
        div(
          idAttr := "generate-p1",
          h2("Party 1"),
        ),
        div(
          idAttr := "generate-cws",
          h2("CWs", cls := List("vocabulary"), title := "Correction Words"),
        ),
        div(
          idAttr := "generate-advice",
          h2("Advice")
        ),
      )
    )
  }

}
