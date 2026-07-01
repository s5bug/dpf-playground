package tf.bug.dpfplayground.state

import cats.effect.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import tf.bug.dpf.impl.{BitVecN, UBitInt}

abstract class SOption {
  def name: String

  type Params

  def defaultParams: Params
  def takeParams(sref: SignallingRef[IO, Params]): Option[Resource[IO, HtmlElement[IO]]]

  type Seed[P <: Params & Singleton]
  def defaultSeed(p: Params): Seed[p.type]
  def takeSeed(p: Params, sref: SignallingRef[IO, Seed[p.type]]): Option[Resource[IO, HtmlElement[IO]]]
}

object SOption {
  val values: Vector[SOption] = Vector(
    Aes128
  )

  case object Aes128 extends SOption {
    override def name: String = "AES (128-bit)"

    // TODO make this take a key
    override type Params = BitVecN[128]
    override def defaultParams: BitVecN[128] = ???
    override def takeParams(sref: SignallingRef[IO, BitVecN[128]]): Option[Resource[IO, HtmlElement[IO]]] = None

    override type Seed[P <: BitVecN[128] & Singleton] = BitVecN[128]
    override def defaultSeed(p: BitVecN[128]): BitVecN[128] = UBitInt[128](0).toBitVecN
    // TODO take a hex string in here
    override def takeSeed(p: BitVecN[128], sref: SignallingRef[IO, BitVecN[128]]): Option[Resource[IO, HtmlElement[IO]]] = None
  }

}
