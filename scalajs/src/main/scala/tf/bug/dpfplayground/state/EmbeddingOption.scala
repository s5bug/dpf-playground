package tf.bug.dpfplayground.state

import cats.effect.{IO, Resource}
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import tf.bug.dpf.Embedding
import tf.bug.dpf.Embedding.Aux
import tf.bug.dpf.impl.{BitInt, BitVecN}
import tf.bug.dpfplayground.state.SOption.Aes128

abstract class EmbeddingOption[
  X <: (XOption { type Params >: XP }) & Singleton,
  XP <: XOption#Params & Singleton,
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

  val x: X
  val xp: XP & x.Params
  val s: S
  val sp: SP & s.Params
  val y: Y
  val yp: YP & y.Params
  
  def evidence(p: Params): Embedding.Aux[x.Instance[xp.type], s.Instance[sp.type], Leaf[p.type], y.Instance[yp.type]]
}

object EmbeddingOption {
  type AuxP[
    X <: (XOption { type Params >: XP }) & Singleton,
    XP <: XOption#Params & Singleton,
    S <: (SOption { type Params >: SP }) & Singleton,
    SP <: SOption#Params & Singleton,
    Y <: (YOption { type Params >: YP }) & Singleton,
    YP <: YOption#Params & Singleton,
    P,
  ] = EmbeddingOption[X, XP, S, SP, Y, YP] { type Params = P }

  // TODO redesign this so casts aren't needed
  def values(x: XOption, xp: x.Params, s: SOption, sp: s.Params, y: YOption, yp: y.Params):
    Vector[EmbeddingOption[x.type, xp.type, s.type, sp.type, y.type, yp.type]] =
    (x, s, y) match {
      case (_, SOption.Aes128, YOption.YOptBitInt) =>
        Vector(
          eOptAdditiveSharePacking(x, xp, sp.asInstanceOf[SOption.Aes128.Params], yp.asInstanceOf[YOption.YOptBitInt.Params])
            .asInstanceOf[EmbeddingOption[x.type, xp.type, s.type, sp.type, y.type, yp.type]]
        )
      case _ => Vector()
    }
    
  final def eOptAdditiveSharePacking(
    xOpt: XOption,
    xParam: xOpt.Params,
    key: BitVecN[128],
    outputWidth: Int,
  ): EmbeddingOption.AuxP[
    xOpt.type,
    xParam.type,
    SOption.Aes128.type,
    key.type & SOption.Aes128.Params,
    YOption.YOptBitInt.type,
    outputWidth.type & YOption.YOptBitInt.Params,
    Unit
  ] = {
    given xDomain: tf.bug.dpf.Domain[xOpt.Instance[xParam.type]] = xOpt.evidenceOfDomain(xParam)
    val evidence0: tf.bug.dpf.Embedding[xOpt.Instance[xParam.type], BitVecN[128], BitInt[outputWidth.type]] =
      tf.bug.dpf.Embedding.additiveSharePacking[xOpt.Instance[xParam.type], 128, outputWidth.type]
    new EmbeddingOption[
      xOpt.type,
      xParam.type,
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

      override val x: xOpt.type = xOpt
      override val xp: xParam.type = xParam
      override final val s = SOption.Aes128
      override val sp: key.type = key
      override final val y = YOption.YOptBitInt
      override val yp: outputWidth.type = outputWidth

      override def evidence(p: Unit): Aux[x.Instance[xParam.type], BitVecN[128], evidence0.L, BitInt[outputWidth.type]] = {
        evidence0
      }
    }
  }
}
