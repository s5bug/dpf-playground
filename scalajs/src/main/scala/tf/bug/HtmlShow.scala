package tf.bug

import calico.html.{Html, Modifier}
import cats.effect.Resource
import cats.effect.Async
import fs2.concurrent.Signal
import fs2.dom.{Element, HtmlElement}
import scodec.bits.BitVector

trait HtmlShow[A] {

  def html[F[_]](as: Signal[F, A])(using async: Async[F]): Resource[F, HtmlElement[F]]

}

object HtmlShow {
  given htmlShowIsSignalModifier[F[_], E <: Element[F], A](using htmlShow: HtmlShow[A], async: Async[F], resourceMod: Modifier[F, E, Resource[F, HtmlElement[F]]]): Modifier[F, E, Signal[F, A]] with {
    def modify(a: Signal[F, A], e: E): Resource[F, Unit] = {
      val resourceElem: Resource[F, HtmlElement[F]] = htmlShow.html[F](a)
      resourceMod.modify(resourceElem, e)
    }
  }

  given htmlShowIsModifier[F[_], E <: Element[F], A](using htmlShow: HtmlShow[A], async: Async[F], resourceMod: Modifier[F, E, Resource[F, HtmlElement[F]]]): Modifier[F, E, A] with {
    def modify(a: A, e: E): Resource[F, Unit] = {
      val resourceElem: Resource[F, HtmlElement[F]] = htmlShow.html[F](Signal.constant(a))
      resourceMod.modify(resourceElem, e)
    }
  }
}
