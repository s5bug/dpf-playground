package tf.bug.dpfplayground

import calico.IOWebApp
import calico.html.io.{*, given}
import calico.html.{HtmlAttr, Modifier}
import cats.effect
import cats.effect.std.{Random, SecureRandom}
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import scodec.bits.BitVector
import tf.bug.dpf.impl.unsafedebug.FreeGroup
import tf.bug.dpf.impl.*
import tf.bug.dpf.{Dpf, Embedding}
import tf.bug.dpfplayground.state.DynamicDpf

object DpfPlayground extends IOWebApp {

  override def render: Resource[effect.IO, HtmlElement[effect.IO]] = for {
    component.Setup(element, result) <- component.Setup()
    r <- {
      div(
        idAttr := "container",
        div(
          idAttr := "left-panel",
          element
        ),
        div(
          idAttr := "right-panel",
          component.Tabs()
        )
      )
    }
  } yield r

}
