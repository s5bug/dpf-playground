package tf.bug.dpf

import cats.Monad
import cats.effect.std.Random
import cats.syntax.invariant.*
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import spire.algebra.Group
import tf.bug.{BitVecN, UBitInt}

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

  private type TupleN[V, C <: Int] <: Tuple = C match {
    case 0 => EmptyTuple
    case compiletime.ops.int.S[n] => V *: TupleN[V, n]
  }

  private given groupForTupleN[V, C <: Int](using gv: Group[V], vc: ValueOf[C]): Group[TupleN[V, C]] with {
    override def empty: TupleN[V, C] = Tuple.fromIArray(IArray.fill[Any](vc.value)(gv.empty)).asInstanceOf[TupleN[V, C]]
    override def inverse(x: TupleN[V, C]): TupleN[V, C] = x.map[[X] =>> V]([T] => (t: T) => gv.inverse(t.asInstanceOf[V])).asInstanceOf[TupleN[V, C]]
    override def combine(x: TupleN[V, C], y: TupleN[V, C]): TupleN[V, C] = x.zip(y).asInstanceOf[TupleN[(V, V), C]].map[[X] =>> V]([T] => (t: T) => {
      val (x, y) = t.asInstanceOf[(V, V)]
      gv.combine(x, y)
    }).asInstanceOf[TupleN[V, C]]
  }

  def fromBitPackable[X, W <: Int, Y](yCodec: Codec[Y])(using xDomain: Domain[X], wValue: ValueOf[W], yGroup: Group[Y]): Embedding[X, BitVecN[W], Y] =
    new ForKnownBitPackable[X, W, Y](xDomain, wValue, yCodec, yGroup)

  private final class ForKnownBitPackable[X, W <: Int, Y](val xDomain: Domain[X], val wValue: ValueOf[W], val yToBits: Codec[Y], val yIsGroup: Group[Y]) extends Embedding[X, BitVecN[W], Y] {
    val yBitWidth: Int = yToBits.sizeBound.exact match {
      case Some(l) => l.toInt
      case None => throw new IllegalArgumentException("Y codec did not have exact known bitwidth")
    }
    // how many Ys can we fit into W bits
    val quotient: Int = wValue.value / yBitWidth
    // yBitWidth * 2^depthOfYinS ≤ sBitWidth
    // i.e. how many layers does the binary tree we pack have
    // depthOfYinS = 0 ⇒ we can only pack one Y in S
    // depthOfYinS = 1 ⇒ we have one boolean decision to make to reach a Y in S
    // depthOfYinS = 2 ⇒ we can pack 4x Y in one S, i.e. 2 boolean decisions
    val depthOfYinS: Int = if wValue.value < yBitWidth then
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

    override type L = TupleN[Y, quotient.type]
    override val lIsGroup: Group[TupleN[Y, quotient.type]] = Embedding.groupForTupleN[Y, quotient.type](using yIsGroup)

    override def lengthen(from: BitVecN[W]): TupleN[Y, quotient.type] = {
      val yBits = scodec.codecs.listOfN(scodec.codecs.provide(quotient), scodec.codecs.bits(yBitWidth))
        .decode(from.raw)
        .toTry
        .get
        .value
      val ys = yBits.map(b => yToBits.decode(b).toTry.get.value)
      Tuple.fromIArray(IArray.from[Any](ys)).asInstanceOf[TupleN[Y, quotient.type]]
    }

    override def embed(at: X, value: Y): TupleN[Y, quotient.type] = {
      val xBits = xToBits(at)
      val xInt = xCodec.decode(xBits).toTry.get.value
      val idx = xInt.toInt % (1 << depthOfYinS)

      val zeroes = lIsGroup.empty
      val arr: Array[Object] = zeroes.toArray
      arr(idx) = value.asInstanceOf[Object]
      Tuple.fromArray(arr).asInstanceOf[TupleN[Y, quotient.type]]
    }
    override def extract(at: X, from: TupleN[Y, quotient.type]): Y = {
      val xBits = xToBits(at)
      val xInt = xCodec.decode(xBits).toTry.get.value
      val idx = xInt.toInt % (1 << depthOfYinS)

      from(idx).asInstanceOf[Y]
    }
  }

}
