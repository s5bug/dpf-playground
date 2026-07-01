package tf.bug.dpfplayground.state

import calico.html.io.{*, given}
import cats.effect.*
import fs2.dom.HtmlElement
import scodec.bits.BitVector
import tf.bug.dpf.Dpf
import tf.bug.dpf.impl.{BitInt, BitVecN, UBitInt}
import tf.bug.dpfplayground.state.DynamicDpf.Default.{embedding, x}

abstract class DynamicDpf {
  val x: XOption
  val s: SOption
  val y: YOption

  val xParam: x.Params
  val sParam: s.Params
  val yParam: y.Params

  // TODO ???
  val embedding: EmbeddingOption[x.type, xParam.type, s.type, sParam.type, y.type, yParam.type]
  val embeddingParam: embedding.Params

  // TODO maybe restrict this to a prepared
  val seed0: s.Seed[sParam.type]
  val seed1: s.Seed[sParam.type]
  val input: x.Domain[xParam.type]
  val output: y.Codomain[yParam.type]

  lazy val dpf: Dpf[x.Domain[xParam.type], s.Seed[sParam.type], embedding.Leaf[embeddingParam.type], y.Codomain[yParam.type]] =
    Dpf.generate(
      Dpf.Prepared(seed0, seed1),
      input,
      output,
      ???,
      ???
    )
}

object DynamicDpf {

  object Default extends DynamicDpf {
    override final val x = XOption.XOptBitInt
    override final val s = SOption.Aes128
    override final val y = YOption.YOptBitInt

    override final val xParam: x.Params & 32 = 32
    override final val sParam: s.Params = UBitInt[128](BigInt("0000deadbeef00000000cafebabe0000", 16)).toBitVecN
    override final val yParam: y.Params & 32 = 32

    override final val embedding =
      EmbeddingOption.eOptAdditiveSharePacking(x, xParam, sParam, yParam)
    override val embeddingParam: Unit = ()

    override val seed0: BitVecN[128] = UBitInt[128](BigInt(0)).toBitVecN
    override val seed1: BitVecN[128] = UBitInt[128](BigInt(0)).toBitVecN
    override val input: BitInt[32] = BitInt[32](0)
    override val output: BitInt[32] = BitInt[32](0)
  }

}
