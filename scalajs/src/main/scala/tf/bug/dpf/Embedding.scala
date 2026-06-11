package tf.bug.dpf

import cats.Monad
import cats.effect.std.Random
import cats.syntax.invariant.*
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import spire.algebra.Group
import spire.math.SafeLong
import tf.bug.{BitVecN, PackedUBitInts, UBitInt}

// we have to steps that occur as transformations out of a DPF's seed block:
// 1. S → L, which may involve calling a PRG to lengthen S
// 2. L → Y^+, for packing one or more Ys into an L
//
// we need to be able to generate correction words on Ls for making them equal except at a given X
abstract class Embedding[X, S, Y] {

  // for a given X, how do we traverse the tree to get to its leaf?
  def directions(x: X): IterableOnce[TreeDirection]

  // L is internal because we may do calculations to compute it (i.e. calculating how many Y can fit in S)
  type L
  val lIsGroup: Group[L]
  // lengthen does not need to be a group homomorphism
  // in fact, embeddings do not need to assume S is a group at all
  def lengthen(from: S): L

  // extract and embed do need to be a group homomorphisms
  val yIsGroup: Group[Y]
  def extract(at: X, from: L): Y
  def embed(at: X, value: Y): L
}

object Embedding {

  type Aux[X, S, L0, Y] = Embedding[X, S, Y] { type L = L0 }

  def xorSharePacking[X, W <: Int, R <: Int](using xDomain: Domain[X], wValue: ValueOf[W], rValue: ValueOf[R]): Embedding[X, BitVecN[W], BitVecN[R]] =
    new XorSharePacking[X, W, R](xDomain, wValue, rValue)

  private final class XorSharePacking[X, W <: Int, R <: Int](val xDomain: Domain[X], val wValue: ValueOf[W], val rValue: ValueOf[R]) extends Embedding[X, BitVecN[W], BitVecN[R]] {
    // TODO move this validation from the contsructor to the method
    // how many Ys can we fit into W bits
    val quotient: Int = wValue.value / rValue.value
    // yBitWidth * 2^depthOfYinS ≤ sBitWidth
    // i.e. how many layers does the binary tree we pack have
    // depthOfYinS = 0 ⇒ we can only pack one Y in S
    // depthOfYinS = 1 ⇒ we have one boolean decision to make to reach a Y in S
    // depthOfYinS = 2 ⇒ we can pack 4x Y in one S, i.e. 2 boolean decisions
    val depthOfYinS: Int = if wValue.value < rValue.value then
      throw new IllegalArgumentException("fromBitPackable is only for the case of Y fitting in S")
    else {
      // if we have 000...001, then numLeadingZeros is 31
      // if we have 000...010, it's 30
      // if we have 000...100, it's 29
      31 - Integer.numberOfLeadingZeros(quotient)
    }

    val xCodec: Codec[UBitInt[xDomain.W]] = UBitInt.codec[xDomain.W](using xDomain.bitwidth)

    def xToBits(x: X): BitVector =
      xCodec.encode(xDomain.indexOf(x)).toTry.get

    override def directions(x: X): IterableOnce[TreeDirection] = {
      val xBits = xToBits(x)
      val xBitsPrefix = xBits.dropRight(depthOfYinS)
      (0 until xBitsPrefix.size.toInt).map(idx => if xBitsPrefix(idx) then TreeDirection.Right else TreeDirection.Left)
    }

    // XOR shared
    override type L = BitVecN[W]
    override val lIsGroup: Group[L] = BitVecN.bitVecNGroup[W](using wValue)
    override val yIsGroup: Group[BitVecN[R]] = BitVecN.bitVecNGroup[R](using rValue)

    override def lengthen(from: BitVecN[W]): L = {
      from
    }

    override def embed(at: X, value: BitVecN[R]): L = {
      val xInt = xDomain.indexOf(at)
      val idx = xInt.toInt % (1 << depthOfYinS)

      val bitIdx = idx * rValue.value
      BitVecN(BitVector.low(bitIdx) ++ BitVector.high(rValue.value) ++ BitVector.low(wValue.value - rValue.value - bitIdx))(using wValue)
    }
    override def extract(at: X, from: L): BitVecN[R] = {
      val xInt = xDomain.indexOf(at)
      val idx = xInt.toInt % (1 << depthOfYinS)

      val bitIdx = idx * rValue.value
      BitVecN(from.raw.slice(bitIdx, bitIdx + wValue.value))(using rValue)
    }
  }

  def uadditiveSharePacking[X, W <: Int, R <: Int](using xDomain: Domain[X], wValue: ValueOf[W], rValue: ValueOf[R]): Embedding[X, BitVecN[W], UBitInt[R]] =
    new UadditiveSharePacking[X, W, R](xDomain, wValue, rValue)

  private final class UadditiveSharePacking[X, W <: Int, R <: Int](val xDomain: Domain[X], val wValue: ValueOf[W], val rValue: ValueOf[R]) extends Embedding[X, BitVecN[W], UBitInt[R]] {
    private inline given wValue.type = wValue
    private inline given rValue.type = rValue

    // TODO move validation from constructor to method
    // how many leaf values can be stored in a block
    val ysPerS: Int = wValue.value / rValue.value
    // how many indexes there are into blocks
    // = xDomain.size.ceilDiv(ysPerS)
    val nonEmptyLeafBlocks: SafeLong = (xDomain.size + (ysPerS - 1)) / ysPerS
    // how many bits are required to select a unique block
    // 1 block → 0 bits
    // 2 blocks → 1 bit
    // 3 blocks → 2 bits
    // 4 blocks → 2 bits
    // 5 thru 8 blocks → 3 bits
    val indexLength: Int = (nonEmptyLeafBlocks - 1).bitLength

    def xToIndexOfBlock(x: X): SafeLong = {
      xDomain.indexOf(x).toSafeLong / ysPerS
    }

    def xToIndexOfY(x: X): Int = {
      (xDomain.indexOf(x).toSafeLong % ysPerS).toInt
    }

    override def directions(x: X): IterableOnce[TreeDirection] = {
      val idx = xToIndexOfBlock(x)
      val bits = UBitInt[indexLength.type](idx.toBigInt).toBitVecN.raw

      (0 until indexLength).map(idx => if bits(idx) then TreeDirection.Right else TreeDirection.Left)
    }

    override type L = PackedUBitInts[R, ysPerS.type]
    override val lIsGroup: Group[PackedUBitInts[R, ysPerS.type]] = PackedUBitInts.group
    override val yIsGroup: Group[UBitInt[R]] = UBitInt.ubitIntGroup

    override def lengthen(from: BitVecN[W]): PackedUBitInts[R, ysPerS.type] = {
      val bits = from.raw
      val bvs = (0 until ysPerS).map(i => bits.slice(i * rValue.value, (1 + i) * rValue.value))
      PackedUBitInts(bvs.map(BitVecN[R](_)).map(_.toUBitInt)*)
    }

    override def embed(at: X, value: UBitInt[R]): PackedUBitInts[R, ysPerS.type] = {
      val zeroes = Vector.fill(ysPerS)(UBitInt[R](0))
      val idx = xToIndexOfY(at)
      PackedUBitInts(zeroes.updated(idx, value)*)
    }

    override def extract(at: X, from: PackedUBitInts[R, ysPerS.type]): UBitInt[R] = {
      val idx = xToIndexOfY(at)
      from.at(idx)
    }
  }

  // TODO is it worth generifying to Prg[BitVecN[W], BitVecN[V]]?
  // biasHelp = how many extra bits to use to make the mapping more balanced
  def rankedGroupBiasedShiftPacking[X, W <: Int, Y](prg: Prg[BitVecN[W], BitVecN[W]], biasHelp: Int)(using xDomain: Domain[X], wValue: ValueOf[W], yDomain: Domain[Y], yGroup: Group[Y]): Embedding[X, BitVecN[W], Y] = {
    // TODO add a parameter for packing nicely (i.e. using lcm(W, Y.bitwidth)/W for number of prg blocks)

    ???
  }

  // biasHelp = how many extra bits to use to make rejection less common
  def rankedGroupRejectionShiftPacking[X, W <: Int, Y](prg: Prg[BitVecN[W], BitVecN[W]], biasHelp: Int)(using xDomain: Domain[X], wValue: ValueOf[W], yDomain: Domain[Y], yGroup: Group[Y]): Embedding[X, BitVecN[W], Y] = {
    ???
  }

}
