package tf.bug.dpfplayground.state

import calico.html.io
import calico.html.io.{*, given}
import cats.effect.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import spire.math.SafeLong
import tf.bug.dpf.impl.{BitInt, UBitInt}

sealed abstract class XOption extends SimpleInstanceOption {
  def evidenceOfDomain(params: this.Params): tf.bug.dpf.Domain[this.Instance[params.type]]
}

object XOption extends SimpleInstanceOption.Companion[XOption] {
  def values: Vector[XOption] = Vector(
    XOptBitInt,
    XOptUBitInt,
  )

  case object XOptBitInt extends XOption {
    override def name: String = "Signed Integer"

    override type Params = Int

    override def defaultParams: Some[Int] = Some(8)
    override def takeParams(sref: SignallingRef[IO, Option[Int]]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        label(forId := "xopt-bitint-bits", "Bits: "),
        input.withSelf { self => (
          labelAttr := "xopt-bitint-bits", `type` := "number",
          minAttr := "1",
          maxAttr := "128",
          value <-- sref.map(_.map(_.toString).getOrElse("")),
          onInput --> { _.evalMap(_ => self.value.get).foreach(v => sref.set(v.toIntOption)) }
        )}
      )
    )

    override type Instance[P <: Int & Singleton] = BitInt[P]
    override def defaultInstance(p: Int): Option[BitInt[p.type]] = if p >= 4 then Some(BitInt[p.type](6)) else None
    override def takeInstance(p: Int, sref: SignallingRef[IO, Option[BitInt[p.type]]]): Option[Resource[IO, HtmlElement[IO]]] = {
      val negativeBound = -(SafeLong(1) << (p - 1))
      val positiveBound = (SafeLong(1) << (p - 1)) - 1
      
      Some(
        div(
          input.withSelf { self => (
            idAttr := "xopt-bitint-point", aria.label := "Input Point", `type` := "number",
            minAttr := negativeBound.toString, maxAttr := positiveBound.toString,
            value <-- sref.map(_.map(_.toSafeLong.toString).getOrElse("")),
            onInput --> { _.evalMap(_ => self.value.get).foreach(v => sref.set(util.Try(BitInt[p.type](BigInt(v))).toOption)) }
          )}
        )
      )
    }

    override def evidenceOfDomain(params: Int): tf.bug.dpf.Domain[BitInt[params.type]] = summon
  }
  case object XOptUBitInt extends XOption {
    override def name: String = "Unsigned Integer"

    override type Params = Int

    override def defaultParams: Some[Int] = Some(8)
    override def takeParams(sref: SignallingRef[IO, Option[Int]]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        label(forId := "xopt-ubitint-bits", "Bits: "),
        input(labelAttr := "xopt-ubitint-bits", `type` := "range", minAttr := "1", maxAttr := "128")
      )
    )

    override type Instance[P <: Int & Singleton] = UBitInt[P]
    override def defaultInstance(p: Int): Option[UBitInt[p.type]] = if p >= 3 then Some(UBitInt[p.type](6)) else None
    override def takeInstance(p: Int, sref: SignallingRef[IO, Option[UBitInt[p.type]]]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        input(idAttr := "xopt-ubitint-domain", aria.label := "Domain", `type` := "number",
          minAttr := "0", maxAttr := ((SafeLong(1) << p) - 1).toString)
      )
    )

    override def evidenceOfDomain(params: Int): tf.bug.dpf.Domain[UBitInt[params.type]] = summon
  }
}
