package tf.bug.dpfplayground.component

import calico.html.{Html, HtmlTag, Modifier, io}
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.implicits.catsSyntaxFlatMapOps
import fs2.dom.HtmlElement
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object Katex {

  private def internal(): js.Promise[Unit] = js.dynamicImport {
    KatexInternal.load
  }.`then`(_.apply())

  def apply[F[_], M](modifier: M)(using async: Async[F], ev: Modifier[F, HtmlElement[F], M]): Resource[F, HtmlElement[F]] = {
    val importElement = async.fromPromise(async.delay(internal()))
    val katex = async.delay(org.scalajs.dom.document.createElement("dpf-katex"))
    Resource.eval(importElement >> katex)
      .asInstanceOf[Resource[F, HtmlElement[F]]]
      .flatTap(ev.modify(modifier, _))
  }

}

private[component] object KatexInternal {
  @js.native
  @JSImport("@/component/katex", JSImport.Default)
  val load: js.Function0[js.Promise[Unit]] = js.native
}
