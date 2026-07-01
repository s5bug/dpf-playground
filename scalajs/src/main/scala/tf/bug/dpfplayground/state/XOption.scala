package tf.bug.dpfplayground.state

import calico.html.io
import calico.html.io.{*, given}
import cats.effect.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import spire.math.SafeLong
import tf.bug.dpf.impl.{BitInt, UBitInt}

sealed abstract class XOption {
  def name: String

  type Params

  def defaultParams: Params
  def takeParams(sref: SignallingRef[IO, Params]): Option[Resource[IO, HtmlElement[IO]]]

  type Domain[P <: Params & Singleton]
  def defaultDomain(p: Params): Domain[p.type]
  def takeDomain(p: Params, sref: SignallingRef[IO, Domain[p.type]]): Option[Resource[IO, HtmlElement[IO]]]
}

object XOption {
  def values: Vector[XOption] = Vector(
    XOptBitInt,
    XOptUBitInt,
  )

  case object XOptBitInt extends XOption {
    override def name: String = "Signed Integer"

    override type Params = Int

    override def defaultParams: Int = 32
    override def takeParams(sref: SignallingRef[IO, Int]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        label(forId := "xopt-bitint-bits", "Bits: "),
        input(labelAttr := "xopt-bitint-bits", `type` := "number", minAttr := "1", maxAttr := "128")
      )
    )

    override type Domain[P <: Int & Singleton] = BitInt[P]
    override def defaultDomain(p: Int): BitInt[p.type] = BitInt[p.type](0)
    override def takeDomain(p: Int, sref: SignallingRef[IO, BitInt[p.type]]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        input.withSelf { self => (
          idAttr := "xopt-bitint-domain", aria.label := "Domain", `type` := "number",
          minAttr := "0", maxAttr := ((SafeLong(1) << p) - 1).toString,
          value <-- sref.map(_.toSafeLong.toString),
          onInput --> { _.evalMap(_ => self.value.get).foreach(v => sref.set(BitInt[p.type](BigInt(v)))) }
        )}
      )
    )
  }
  case object XOptUBitInt extends XOption {
    override def name: String = "Unsigned Integer"

    override type Params = Int

    override def defaultParams: Int = 32
    override def takeParams(sref: SignallingRef[IO, Int]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        label(forId := "xopt-ubitint-bits", "Bits: "),
        input(labelAttr := "xopt-ubitint-bits", `type` := "range", minAttr := "1", maxAttr := "128")
      )
    )

    override type Domain[P <: Int & Singleton] = UBitInt[P]
    override def defaultDomain(p: Int): UBitInt[p.type] = UBitInt[p.type](0)
    override def takeDomain(p: Int, sref: SignallingRef[IO, UBitInt[p.type]]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        input(idAttr := "xopt-ubitint-domain", aria.label := "Domain", `type` := "number",
          minAttr := "0", maxAttr := ((SafeLong(1) << p) - 1).toString)
      )
    )
  }
}
