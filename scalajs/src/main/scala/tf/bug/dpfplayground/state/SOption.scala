package tf.bug.dpfplayground.state

import calico.html.io.{*, given}
import cats.effect.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import scodec.bits.BitVector
import tf.bug.dpf.impl.{BitVecN, UBitInt}

sealed abstract class SOption extends SimpleInstanceOption {
  def evidenceOfSeeding(params: this.Params): tf.bug.dpf.Seeding[this.Instance[params.type]]
}

object SOption extends SimpleInstanceOption.Companion[SOption] {
  val values: Vector[SOption] = Vector(
    Aes128
  )

  case object Aes128 extends SOption {
    override def name: String = "AES (128-bit)"

    override type Params = BitVecN[128]
    // TODO give a default AES key
    override def defaultParams: Option[BitVecN[128]] =
      Some(UBitInt[128](BigInt("09f911029d74e35bd84156c5635688c0", 16)).toBitVecN)
    // TODO make this accept an AES key
    override def takeParams(sref: SignallingRef[IO, Option[BitVecN[128]]]): Option[Resource[IO, HtmlElement[IO]]] = Some(
      div(
        label(forId := "sopt-aes-key", "Key: "),
        input.withSelf { self => (
          labelAttr := "sopt-aes-key", `type` := "text",
          pattern := "[0-9a-fA-F]{32}",
          value <-- sref.map(_.map(_.raw.toHex)),
          onInput --> {
            _.evalMap(_ => self.value.get)
              .map(s => BitVector.fromHex(s))
              .map(_.filter(_.length == 128))
              .foreach(v => sref.set(v.map(BitVecN(_)))) }
        )}
      )
    )

    override type Instance[P <: BitVecN[128] & Singleton] = BitVecN[128]
    override def defaultInstance(p: BitVecN[128]): Option[BitVecN[128]] =
      Some(UBitInt[128](BigInt("0123456789abcdeffedcba9876543210", 16)).toBitVecN)
    // TODO take a hex string in here
    override def takeInstance(p: BitVecN[128], sref: SignallingRef[IO, Option[BitVecN[128]]]): Option[Resource[IO, HtmlElement[IO]]] =
      Some(
        div(
          input.withSelf { self =>
            (
              aria.label := "128-bit Seed", `type` := "text",
              pattern := "[0-9a-fA-F]{32}",
              value <-- sref.map(_.map(_.raw.toHex)),
              onInput --> {
                _.evalMap(_ => self.value.get)
                  .map(s => BitVector.fromHex(s))
                  .map(_.filter(_.length == 128))
                  .foreach(v => sref.set(v.map(BitVecN(_))))
              }
            )
          }
          // TODO add randomize button
        )
      )

    override def evidenceOfSeeding(params: BitVecN[128]): tf.bug.dpf.Seeding[BitVecN[128]] =
      tf.bug.dpf.impl.AesPrg(params).Seeding
  }

}
