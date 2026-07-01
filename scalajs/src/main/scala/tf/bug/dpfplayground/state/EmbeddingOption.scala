package tf.bug.dpfplayground.state

import tf.bug.dpf.Embedding
import tf.bug.dpf.Embedding.Aux
import tf.bug.dpf.impl.{BitInt, BitVecN}

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
  
  val x: X
  val xp: XP & x.Params
  val s: S
  val sp: SP & s.Params
  val y: Y
  val yp: YP & y.Params
  
  def evidence(p: Params): Embedding.Aux[x.Domain[xp.type], s.Seed[sp.type], Leaf[p.type], y.Codomain[yp.type]]
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
  
  def values(x: XOption, xp: x.Params, s: SOption, sp: s.Params, y: YOption, yp: y.Params):
    Vector[EmbeddingOption[x.type, xp.type, s.type, sp.type, y.type, yp.type]] =
    Vector(
      // TODO
    )
    
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
      override type Leaf[P <: Params & Singleton] = Nothing
      
      override val x: xOpt.type = xOpt
      override val xp: xParam.type = xParam
      override final val s = SOption.Aes128
      override val sp: key.type = key
      override final val y = YOption.YOptBitInt
      override val yp: outputWidth.type = outputWidth

      override def evidence(p: Unit): Aux[x.Domain[xParam.type], BitVecN[128], Nothing, BitInt[outputWidth.type]] = {
        ???
      }
    }
  }
}
