package tf.bug.dpfplayground.state

import calico.html.io.{*, given}
import cats.effect.*
import cats.effect.kernel.Resource
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import tf.bug.dpf.impl.BitInt

abstract class YOption {
  def name: String

  type Params
  
  def defaultParams: Params
  def takeParams(sref: SignallingRef[IO, Params]): Option[Resource[IO, HtmlElement[IO]]]

  type Codomain[P <: Params & Singleton]
  def defaultCodomain(params: Params): Codomain[params.type]
  def takeCodomain(params: Params, sref: SignallingRef[IO, Params]): Option[Resource[IO, HtmlElement[IO]]]
}

object YOption {
  val values: Vector[YOption] = Vector(
    YOptBitInt
  )
  
  case object YOptBitInt extends YOption {
    override def name: String = "Signed Integer"

    override type Params = Int

    override def defaultParams: Int = 32
    override def takeParams(sref: SignallingRef[IO, Int]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        label(forId := "yopt-bitint-bits", "Bits: "),
        input(labelAttr := "yopt-bitint-bits", `type` := "number", minAttr := "1", maxAttr := "128")
      )
    )

    override type Codomain[P <: Params & Singleton] = BitInt[P]
    override def defaultCodomain(params: Int): BitInt[params.type] = BitInt[params.type](0)
    // TODO do this shit
    override def takeCodomain(params: Int, sref: SignallingRef[IO, Int]): Option[Resource[IO, HtmlElement[IO]]] = None
  }
}
