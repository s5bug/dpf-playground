package tf.bug.dpfplayground.state

import cats.effect.{IO, Resource}
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import tf.bug.dpf.Embedding
import tf.bug.dpf.Embedding.Aux
import tf.bug.dpf.impl.{BitInt, BitVecN}
import tf.bug.dpfplayground.state.SOption.Aes128

abstract class EmbeddingOption[
  S <: (SOption { type Params >: SP }) & Singleton,
  SP <: SOption#Params & Singleton,
  Y <: (YOption { type Params >: YP }) & Singleton,
  YP <: YOption#Params & Singleton,
] {
  type Params
  type Leaf[P <: Params & Singleton]

  def name: String
  def defaultParams: Option[Params]
  // TODO make this return a signal instead of take a SignallingRef
  def takeParams(sref: SignallingRef[IO, Option[Params]]): Option[Resource[IO, HtmlElement[IO]]]

  val s: S
  val sp: SP & s.Params
  val y: Y
  val yp: YP & y.Params
  
  def evidence(p: Params): Embedding.Aux[s.Instance[sp.type], Leaf[p.type], y.Instance[yp.type]]
}

object EmbeddingOption {
  type AuxP[
    S <: (SOption { type Params >: SP }) & Singleton,
    SP <: SOption#Params & Singleton,
    Y <: (YOption { type Params >: YP }) & Singleton,
    YP <: YOption#Params & Singleton,
    P,
  ] = EmbeddingOption[S, SP, Y, YP] { type Params = P }

  // TODO redesign this so casts aren't needed
  def values(s: SOption, sp: s.Params, y: YOption, yp: y.Params):
    Vector[EmbeddingOption[s.type, sp.type, y.type, yp.type]] =
    (s, y) match {
      case (SOption.Aes128, YOption.YOptBitInt) =>
        Vector(
          eOptAdditiveSharePacking(sp.asInstanceOf[SOption.Aes128.Params], yp.asInstanceOf[YOption.YOptBitInt.Params])
            .asInstanceOf[EmbeddingOption[s.type, sp.type, y.type, yp.type]]
        )
      case _ => Vector()
    }
    
  final def eOptAdditiveSharePacking(
    key: BitVecN[128],
    outputWidth: Int,
  ): EmbeddingOption.AuxP[
    SOption.Aes128.type,
    key.type & SOption.Aes128.Params,
    YOption.YOptBitInt.type,
    outputWidth.type & YOption.YOptBitInt.Params,
    Unit
  ] = {
    val evidence0: tf.bug.dpf.Embedding[ BitVecN[128], BitInt[outputWidth.type]] =
      tf.bug.dpf.Embedding.additiveSharePacking[128, outputWidth.type]
    new EmbeddingOption[
      SOption.Aes128.type,
      key.type & SOption.Aes128.Params,
      YOption.YOptBitInt.type,
      outputWidth.type & YOption.YOptBitInt.Params
    ] {
      override type Params = Unit
      // TODO fixme
      override type Leaf[P <: Params & Singleton] = evidence0.L

      override def name: String = "Packed Ints in AES blocks"
      override def defaultParams: Some[Unit] = Some(())
      override def takeParams(sref: SignallingRef[IO, Option[Unit]]): Option[Resource[IO, HtmlElement[IO]]] = None

      override final val s = SOption.Aes128
      override val sp: key.type = key
      override final val y = YOption.YOptBitInt
      override val yp: outputWidth.type = outputWidth

      override def evidence(p: Unit): Aux[BitVecN[128], evidence0.L, BitInt[outputWidth.type]] = {
        evidence0
      }
    }
  }
}
