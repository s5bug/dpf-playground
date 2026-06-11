package tf.bug

import cats.syntax.group.*
import scodec.bits.BitVector
import spire.algebra.Group
import tf.bug.{BitInt, BitVecN, UBitInt}

opaque type PackedUBitInts[W <: Int, N <: Int] = BitVector

object PackedUBitInts {
  
  def apply[W <: Int, N <: Int](coll: UBitInt[W]*)(using wValue: ValueOf[W], nValue: ValueOf[N]): PackedUBitInts[W, N] =
    if coll.sizeIs != nValue.value then throw new IllegalArgumentException(s"expected collection of length ${nValue.value}, found ${coll.size}")
    else coll.map(_.toBitVecN.raw).reduce(_ ++ _)
  
  extension[W <: Int, N <: Int] (p: PackedUBitInts[W, N])(using wValue: ValueOf[W], nValue: ValueOf[N]) {
    def at(x: Int): UBitInt[W] = BitVecN[W](p.slice(wValue.value * x, wValue.value * (1 + x))).toUBitInt
    def unpack: Vector[UBitInt[W]] = (0 until nValue.value).map(at).toVector
  }
  
  given group[W <: Int, N <: Int](using wValue: ValueOf[W], nValue: ValueOf[N]): Group[PackedUBitInts[W, N]] with {
    def empty: PackedUBitInts[W, N] = BitVector.low(wValue.value * nValue.value)
    def inverse(a: PackedUBitInts[W, N]): PackedUBitInts[W, N] = apply(a.unpack.map(_.inverse())*)
    def combine(x: PackedUBitInts[W, N], y: PackedUBitInts[W, N]): PackedUBitInts[W, N] =
      apply(x.unpack.zip(y.unpack).map(_ |+| _)*)
  }
  
}

opaque type PackedBitInts[W <: Int, N <: Int] = BitVector

object PackedBitInts {

  def apply[W <: Int, N <: Int](coll: BitInt[W]*)(using wValue: ValueOf[W], nValue: ValueOf[N]): PackedBitInts[W, N] =
    if coll.sizeIs != nValue.value then throw new IllegalArgumentException(s"expected collection of length ${nValue.value}, found ${coll.size}")
    else coll.map(_.toBitVecN.raw).reduce(_ ++ _)

  extension[W <: Int, N <: Int] (p: PackedBitInts[W, N])(using wValue: ValueOf[W], nValue: ValueOf[N]) {
    def at(x: Int): BitInt[W] = BitVecN[W](p.slice(wValue.value * x, wValue.value * (1 + x))).toBitInt
    def unpack: Vector[BitInt[W]] = (0 until nValue.value).map(at).toVector
  }

  given group[W <: Int, N <: Int](using wValue: ValueOf[W], nValue: ValueOf[N]): Group[PackedBitInts[W, N]] with {
    def empty: PackedBitInts[W, N] = BitVector.low(wValue.value * nValue.value)
    def inverse(a: PackedBitInts[W, N]): PackedBitInts[W, N] = apply(a.unpack.map(_.inverse())*)
    def combine(x: PackedBitInts[W, N], y: PackedBitInts[W, N]): PackedBitInts[W, N] =
      apply(x.unpack.zip(y.unpack).map(_ |+| _)*)
  }

}
