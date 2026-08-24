package tf.bug.dpf

import cats.Monad
import cats.effect.std.Random
import cats.syntax.invariant.*
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import spire.algebra.Group
import spire.math.SafeLong
import tf.bug.dpf.impl.{BitInt, BitVecN, PackedBitInts, PackedUBitInts, UBitInt}

// we have to steps that occur as transformations out of a DPF's seed block:
// 1. S → L, which may involve calling a PRG to lengthen S
// 2. L → Y^+, for packing one or more Ys into an L
//
// we need to be able to generate correction words on Ls for making them equal except at a given X
abstract class Embedding[S, Y] {
  
  // how many bits of X's domain do we need to traverse the tree?
  def treeDepth[X](using domain: Domain[X]): Int
  final def directions[X](at: X)(using domain: Domain[X]): IterableOnce[TreeDirection] = {
    domain.indexOf(at).toBitVecN(using domain.bitwidth).raw.toIndexedSeq.view.map {
      case false => TreeDirection.Left
      case true => TreeDirection.Right
    }
  }

  // L is internal because we may do calculations to compute it (i.e. calculating how many Y can fit in S)
  type L
  val lIsGroup: Group[L]
  // lengthen does not need to be a group homomorphism
  // in fact, embeddings do not need to assume S is a group at all
  def lengthen(from: S): L

  // extract and embed do need to be a group homomorphisms
  val yIsGroup: Group[Y]
  def extract[X](at: X, from: L)(using domain: Domain[X]): Y
  def embed[X](at: X, value: Y)(using domain: Domain[X]): L
}

object Embedding {

  type Aux[S, L0, Y] = Embedding[S, Y] { type L = L0 }

  def xorSharePacking[W <: Int, R <: Int](using wValue: ValueOf[W], rValue: ValueOf[R]): Embedding[BitVecN[W], BitVecN[R]] =
    new XorSharePacking[W, R](wValue, rValue)

  private final class XorSharePacking[W <: Int, R <: Int](val wValue: ValueOf[W], val rValue: ValueOf[R]) extends Embedding[BitVecN[W], BitVecN[R]] {
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

    override def treeDepth[X](using domain: Domain[X]): Int =
      domain.bitwidth.value - depthOfYinS
    
    // XOR shared
    override type L = BitVecN[W]
    override val lIsGroup: Group[L] = BitVecN.bitVecNGroup[W](using wValue)
    override val yIsGroup: Group[BitVecN[R]] = BitVecN.bitVecNGroup[R](using rValue)

    override def lengthen(from: BitVecN[W]): L = {
      from
    }

    override def embed[X](at: X, value: BitVecN[R])(using xDomain: Domain[X]): L = {
      val xInt = xDomain.indexOf(at)
      val idx = xInt.toInt % (1 << depthOfYinS)

      val bitIdx = idx * rValue.value
      BitVecN(BitVector.low(bitIdx) ++ BitVector.high(rValue.value) ++ BitVector.low(wValue.value - rValue.value - bitIdx))(using wValue)
    }
    override def extract[X](at: X, from: L)(using xDomain: Domain[X]): BitVecN[R] = {
      val xInt = xDomain.indexOf(at)
      val idx = xInt.toInt % (1 << depthOfYinS)

      val bitIdx = idx * rValue.value
      BitVecN(from.raw.slice(bitIdx, bitIdx + wValue.value))(using rValue)
    }
  }

  def uadditiveSharePacking[W <: Int, R <: Int](using wValue: ValueOf[W], rValue: ValueOf[R]): Embedding[BitVecN[W], UBitInt[R]] =
    new UadditiveSharePacking[W, R](wValue, rValue)

  private final class UadditiveSharePacking[W <: Int, R <: Int](val wValue: ValueOf[W], val rValue: ValueOf[R]) extends Embedding[BitVecN[W], UBitInt[R]] {
    private inline given wValue.type = wValue
    private inline given rValue.type = rValue

    // how many leaf values can be stored in a block
    val ysPerS: Int = wValue.value / rValue.value

    def xToIndexOfY[X](x: X)(using xDomain: Domain[X]): Int = {
      (xDomain.indexOf(x).toSafeLong % ysPerS).toInt
    }

    override def treeDepth[X](using xDomain: Domain[X]): Int = {
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
      
      indexLength
    }

    override type L = PackedUBitInts[R, ysPerS.type]
    override val lIsGroup: Group[PackedUBitInts[R, ysPerS.type]] = PackedUBitInts.group
    override val yIsGroup: Group[UBitInt[R]] = UBitInt.ubitIntGroup

    override def lengthen(from: BitVecN[W]): PackedUBitInts[R, ysPerS.type] = {
      val bits = from.raw
      val bvs = (0 until ysPerS).map(i => bits.slice(i * rValue.value, (1 + i) * rValue.value))
      PackedUBitInts(bvs.map(BitVecN[R](_)).map(_.toUBitInt)*)
    }

    override def embed[X](at: X, value: UBitInt[R])(using xDomain: Domain[X]): PackedUBitInts[R, ysPerS.type] = {
      val zeroes = Vector.fill(ysPerS)(UBitInt[R](0))
      val idx = xToIndexOfY(at)
      PackedUBitInts(zeroes.updated(idx, value)*)
    }

    override def extract[X](at: X, from: PackedUBitInts[R, ysPerS.type])(using xDomain: Domain[X]): UBitInt[R] = {
      val idx = xToIndexOfY(at)
      from.at(idx)
    }
  }

  // TODO fix code duplication here wrt UBitInt/BitInt, maybe using Codec?
  def additiveSharePacking[W <: Int, R <: Int](using wValue: ValueOf[W], rValue: ValueOf[R]): Embedding[BitVecN[W], BitInt[R]] =
    new AdditiveSharePacking[W, R](wValue, rValue)

  private final class AdditiveSharePacking[W <: Int, R <: Int](val wValue: ValueOf[W], val rValue: ValueOf[R]) extends Embedding[BitVecN[W], BitInt[R]] {
    private inline given wValue.type = wValue

    private inline given rValue.type = rValue

    // how many leaf values can be stored in a block
    val ysPerS: Int = wValue.value / rValue.value

    def xToIndexOfY[X](x: X)(using xDomain: Domain[X]): Int = {
      (xDomain.indexOf(x).toSafeLong % ysPerS).toInt
    }

    override def treeDepth[X](using xDomain: Domain[X]): Int = {
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

      indexLength
    }

    override type L = PackedBitInts[R, ysPerS.type]
    override val lIsGroup: Group[PackedBitInts[R, ysPerS.type]] = PackedBitInts.group
    override val yIsGroup: Group[BitInt[R]] = BitInt.bitIntGroup

    override def lengthen(from: BitVecN[W]): PackedBitInts[R, ysPerS.type] = {
      val bits = from.raw
      val bvs = (0 until ysPerS).map(i => bits.slice(i * rValue.value, (1 + i) * rValue.value))
      PackedBitInts(bvs.map(BitVecN[R](_)).map(_.toBitInt) *)
    }

    override def embed[X](at: X, value: BitInt[R])(using xDomain: Domain[X]): PackedBitInts[R, ysPerS.type] = {
      val zeroes = Vector.fill(ysPerS)(BitInt[R](0))
      val idx = xToIndexOfY(at)
      PackedBitInts(zeroes.updated(idx, value) *)
    }

    override def extract[X](at: X, from: PackedBitInts[R, ysPerS.type])(using xDomain: Domain[X]): BitInt[R] = {
      val idx = xToIndexOfY(at)
      from.at(idx)
    }
  }

  // TODO is it worth generifying to Prg[BitVecN[W], BitVecN[V]]?
  // biasHelp = how many extra bits to use to make the mapping more balanced
  def rankedGroupBiasedShiftPacking[W <: Int, Y](prg: Prg[BitVecN[W], BitVecN[W]], biasHelp: Int)(using wValue: ValueOf[W], yDomain: Domain[Y], yGroup: Group[Y]): Embedding[BitVecN[W], Y] = {
    // TODO add a parameter for packing nicely (i.e. using lcm(W, Y.bitwidth)/W for number of prg blocks)

    ???
  }

  // biasHelp = how many extra bits to use to make rejection less common
  def rankedGroupRejectionShiftPacking[W <: Int, Y](prg: Prg[BitVecN[W], BitVecN[W]], biasHelp: Int)(using wValue: ValueOf[W], yDomain: Domain[Y], yGroup: Group[Y]): Embedding[BitVecN[W], Y] = {
    ???
  }

}
