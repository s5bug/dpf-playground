package tf.bug.dpf.impl

import cats.Monad
import cats.effect.std.Random
import cats.syntax.functor.*
import tf.bug.{BitVecN, UBitInt}
import tf.bug.dpf.{Correctable, Seeding}

final class SBoxPrg[W <: Int] private (val box: IndexedSeq[BitVecN[W]])(using w: ValueOf[W]) extends Seeding[BitVecN[W]] {
  override val seedIsCorrectable: Correctable[BitVecN[W]] = summon
  
  def expand(root: BitVecN[W]): (BitVecN[W], BitVecN[W]) = {
    val leftIn = root.toUBitInt
    val rightIn = root.toUBitInt ^ UBitInt[W](1)

    (box(leftIn.toInt), box(rightIn.toInt))
  }
}

object SBoxPrg {
  def apply[W <: Int](box: IndexedSeq[UBitInt[W]])(using ev: ValueOf[W]): SBoxPrg[W] = {
    val count = 1 << ev.value
    if box.sizeIs != count then
      throw new IllegalArgumentException(s"sbox box too small (size ${box.size}) for width ${ev.value} (expected size ${count})")

    new SBoxPrg[W](box.map(_.toBitVecN))
  }
}
