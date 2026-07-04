package tf.bug.dpfplayground.state

import calico.html.io.{*, given}
import cats.effect.*
import cats.effect.kernel.Resource
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import spire.math.SafeLong
import tf.bug.dpf.impl.BitInt

abstract class YOption extends SimpleInstanceOption {
  def evidenceOfGroup(params: this.Params): spire.algebra.Group[this.Instance[params.type]]
}

object YOption extends SimpleInstanceOption.Companion[YOption] {
  val values: Vector[YOption] = Vector(
    YOptBitInt
  )
  
  case object YOptBitInt extends YOption {
    override def name: String = "Signed Integer"

    override type Params = Int

    override def defaultParams: Some[Int] = Some(8)
    override def takeParams(sref: SignallingRef[IO, Option[Int]]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        label(forId := "yopt-bitint-bits", "Bits: "),
        input.withSelf { self => (
          labelAttr := "yopt-bitint-bits", `type` := "number",
          minAttr := "1",
          maxAttr := "128",
          value <-- sref.map(_.map(_.toString).getOrElse("")),
          onInput --> { _.evalMap(_ => self.value.get).foreach(v => sref.set(v.toIntOption)) }
        )}
      )
    )

    override type Instance[P <: Params & Singleton] = BitInt[P]
    override def defaultInstance(params: Int): Option[BitInt[params.type]] = if params >= 4 then Some(BitInt[params.type](7)) else None
    override def takeInstance(params: Int, sref: SignallingRef[IO, Option[BitInt[params.type]]]): Option[Resource[IO, HtmlElement[IO]]] = {
      val negativeBound = -(SafeLong(1) << (params - 1))
      val positiveBound = (SafeLong(1) << (params - 1)) - 1

      Some(
        div(
          input.withSelf { self => (
            idAttr := "yopt-bitint-value", aria.label := "Output Value", `type` := "number",
            minAttr := negativeBound.toString, maxAttr := positiveBound.toString,
            value <-- sref.map(_.map(_.toSafeLong.toString).getOrElse("")),
            onInput --> { _.evalMap(_ => self.value.get).foreach(v => sref.set(util.Try(BitInt[params.type](BigInt(v))).toOption)) }
          )}
        )
      )
    }

    override def evidenceOfGroup(params: Int): spire.algebra.Group[BitInt[params.type]] =
      summon
  }
}
