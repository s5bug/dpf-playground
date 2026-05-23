package tf.bug.dpf.impl

import cats.Monad
import cats.effect.std.Random
import cats.syntax.functor.*
import tf.bug.{BitVecN, UBitInt}
import tf.bug.dpf.{Correctable, Seeding}

object Ranqd1 extends Seeding[Advised[UBitInt[8]]] {

  final val a = 1664525
  final val c = 1013904223

  override val seedIsCorrectable: Correctable[Advised[UBitInt[8]]] = summon
  
  override def expand(root: Advised[UBitInt[8]]): (Advised[UBitInt[8]], Advised[UBitInt[8]]) = {
    val asInt = root.element.toInt << 2 | (if root.party0 then 2 else 0) | (if root.party1 then 1 else 0)

    val state = (asInt * a) + c
    val out0 = UBitInt[8](state >>> 24)
    val out0p0 = (state & 0x4000) != 0
    val out0p1 = (state & 0x2000) != 0

    val state2 = (state * a) + c
    val out1 = UBitInt[8](state2 >>> 24)
    val out1p0 = (state2 & 0x4000) != 0
    val out1p1 = (state2 & 0x2000) != 0

    (Advised(out0, out0p0, out0p1), Advised(out1, out1p0, out1p1))
  }

}
