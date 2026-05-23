package tf.bug.dpf

import cats.Monad
import cats.effect.std.Random
import cats.syntax.invariant.*
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import spire.algebra.Group
import tf.bug.{BitVecN, UBitInt}

// if the DPF has a domain of X, nodes of S, and an output of Y
// this describes
// 1. how to store a Y at an X in an S
// 2. how to recover a share of Y from an S at an X
abstract class Embedding[X, S, Y] {

  // for a given X, how do we traverse the tree to get to its leaf?
  def directions(x: X): IterableOnce[TreeDirection]

  val yIsGroup: Group[Y]

  // should be a "balanced function"
  // ideally:
  // - if |S|/at ≤ |Y|, then extract must be injective
  // - otherwise, ideally |S|/at = n|Y|, where extract's preimages all have the same size
  // sometimes this isn't possible (i.e. S is bits and Y is a non-power-of-two-size thing)
  // in those cases, one can define an embedding that uses "more of" S to determine the Y
  // e.g. for S_3 in BitVecN[3], the two values 110_2 and 111_2 out of 8 total have to be mapped to something, making 25% inbalance
  // but increase the bits used, i.e. BitVecN[5], and now only the values 11110_2 and 11111_2 out of 32 total have a duplicate mapping
  def extract(at: X, from: S): Y
}

object Embedding {

  def shareableOnDomain[X, S](using xDomain: Domain[X], sShares: Group[S], sSampleable: Sampleable[S]): Embedding[X, S, S] =
    new ForShareableOnDomain[X, S](xDomain, sShares, sSampleable)

  private final class ForShareableOnDomain[X, S](val xDomain: Domain[X], val sShares: Group[S], val sSampleable: Sampleable[S]) extends Embedding[X, S, S] {
    val xCodec: Codec[UBitInt[xDomain.W]] = UBitInt.codec[xDomain.W](using xDomain.bitwidth)

    def xToBits(x: X): BitVector =
      xCodec.encode(xDomain.indexOf(x)).toTry.get

    override def directions(x: X): IterableOnce[TreeDirection] = {
      val xBits = xToBits(x)
      (0 until xBits.size.toInt).map(idx => if xBits(idx) then TreeDirection.Right else TreeDirection.Left)
    }

    override val yIsGroup: Group[S] = sShares

    override def extract(at: X, from: S): S = from
  }

  def fromBitPackable[X, S, Y](cs: Codec[S], cy: Codec[Y])(using xDomain: Domain[X]): Embedding[X, S, Y] =
    new ForKnownBitPackable[X, S, Y](xDomain, cs, cy)

  private final class ForKnownBitPackable[X, S, Y](val xDomain: Domain[X], val sToBits: Codec[S], val yToBits: Codec[Y]) extends Embedding[X, S, Y] {
    // TODO figure out the code duplication here
    val sBitWidth: Int = sToBits.sizeBound.exact match {
      case Some(l) => l.toInt
      case None => throw new IllegalArgumentException("S codec did not have exact known bitwidth")
    }
    val yBitWidth: Int = yToBits.sizeBound.exact match {
      case Some(l) => l.toInt
      case None => throw new IllegalArgumentException("Y codec did not have exact known bitwidth")
    }
    // yBitWidth * 2^depthOfYinS ≤ sBitWidth
    // i.e. how many layers does the binary tree we pack have
    // depthOfYinS = 0 ⇒ we can only pack one Y in S
    // depthOfYinS = 1 ⇒ we have one boolean decision to make to reach a Y in S
    // depthOfYinS = 2 ⇒ we can pack 4x Y in one S, i.e. 2 boolean decisions
    val depthOfYinS: Int = if sBitWidth < yBitWidth then
      throw new IllegalArgumentException("fromBitPackable is only for the case of Y fitting in S")
    else {
      // how many Ys can we pack
      val quotient = sBitWidth / yBitWidth
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

    def yToBits(y: Y): BitVecN[yBitWidth.type] =
      BitVecN(yToBits.encode(y).toTry.get)

    def yFromBits(yBits: BitVecN[yBitWidth.type]): Y =
      yToBits.decode(yBits.raw).flatMap { value =>
        if value.remainder.isEmpty then Attempt.Successful(value.value)
        else Attempt.Failure(scodec.Err(s"${value.remainder.size} bits leftover when decoding"))
      }.toTry.get

    override val yIsGroup: Group[Y] =
      BitVecN.bitVecNGroup[yBitWidth.type].imap(yFromBits)(yToBits)

    override def extract(at: X, from: S): Y = {
      val xBits = xToBits(at)
      val xBitsSuffix = xBits.takeRight(depthOfYinS)
      val index = BitVecN[depthOfYinS.type](xBitsSuffix).toUBitInt
      val bitOffset = yBitWidth * index.toInt

      val sBits = sToBits.encode(from).toTry.get
      val yBits = BitVecN[yBitWidth.type](sBits.drop(bitOffset).take(yBitWidth))
      yFromBits(yBits)
    }
  }

  def fromDomain[X, S, Y](cs: Codec[S], extraBits: Int)(using xDomain: Domain[X], yDomain: Domain[Y], yGroup: Group[Y]): Embedding[X, S, Y] =
    new ForDomain[X, S, Y](xDomain, cs, yDomain, yGroup, extraBits)

  private final class ForDomain[X, S, Y](val xDomain: Domain[X], val sToBits: Codec[S], val yDomain: Domain[Y], val yIsGroup: Group[Y], val extraBits: Int) extends Embedding[X, S, Y] {
    // TODO figure out the code duplication here
    val sBitWidth: Int = sToBits.sizeBound.exact match {
      case Some(l) => l.toInt
      case None => throw new IllegalArgumentException("S codec did not have exact known bitwidth")
    }
    val yBitWidth: Int = yDomain.bitwidth.value + extraBits

    val depthOfYinS: Int = if sBitWidth < yBitWidth then
      throw new IllegalArgumentException("Y bitWidth + extraBits doesn't fit in S")
    else {
      // how many Ys can we pack
      val quotient = sBitWidth / yBitWidth
      // if we have 000...001, then numLeadingZeros is 31
      // if we have 000...010, it's 30
      // if we have 000...100, it's 29
      31 - Integer.numberOfLeadingZeros(quotient)
    }

    val yMinIndex: UBitInt[yBitWidth.type] =
      yDomain.indexOf(yDomain.bounded.minBound)
        // yBitWidth ≥ yDomain.W
        .asInstanceOf[UBitInt[yBitWidth.type]]
    val yMaxIndex: UBitInt[yBitWidth.type] =
      yDomain.indexOf(yDomain.bounded.maxBound)
        // yBitWidth ≥ yDomain.W
        .asInstanceOf[UBitInt[yBitWidth.type]]

    val preModulus: UBitInt[yBitWidth.type] = yMaxIndex - yMinIndex
    val negativeOne: UBitInt[yBitWidth.type] = UBitInt[yBitWidth.type](0) - UBitInt[yBitWidth.type](1)
    val modulus: UBitInt[yBitWidth.type] = preModulus + UBitInt[yBitWidth.type](1)

    val xCodec: Codec[UBitInt[xDomain.W]] = UBitInt.codec[xDomain.W](using xDomain.bitwidth)

    def xToBits(x: X): BitVector =
      xCodec.encode(xDomain.indexOf(x)).toTry.get

    override def directions(x: X): IterableOnce[TreeDirection] = {
      val xBits = xToBits(x)
      val xBitsPrefix = xBits.dropRight(depthOfYinS)
      (0 until xBitsPrefix.size.toInt).map(idx => if xBitsPrefix(idx) then TreeDirection.Right else TreeDirection.Left)
    }

    override def extract(at: X, from: S): Y = {
      val xBits = xToBits(at)
      val xBitsSuffix = xBits.takeRight(depthOfYinS)
      val index = BitVecN[depthOfYinS.type](xBitsSuffix).toUBitInt
      val bitOffset = yBitWidth * index.toInt

      val sBits = sToBits.encode(from).toTry.get
      val yBits = sBits.drop(bitOffset).take(yBitWidth)
      val yBitVecN: BitVecN[yBitWidth.type] = BitVecN(yBits)
      val asNumber: UBitInt[yBitWidth.type] = yBitVecN.toUBitInt

      val inRange = if preModulus == negativeOne then {
        yMinIndex + (asNumber & negativeOne)
      } else {
        yMinIndex + (asNumber % modulus)
      }

      // we can downcast inRange back to the original domain width safely
      val downcast = inRange.asInstanceOf[UBitInt[yDomain.W]]
      yDomain(downcast)
    }
  }

}
