package tf.bug.dpf

import cats.kernel.BoundedEnumerable
import spire.math.SafeLong
import tf.bug.UBitInt

trait Domain[A] {

  type W <: Int
  given bitwidth: ValueOf[W] = compiletime.deferred

  given bounded: BoundedEnumerable[A] = compiletime.deferred

  // indexOf and apply must be inverses and strictly monotonic
  // i.e. both of the following must hold to be sufficient:
  // 1. apply(indexOf(a)) = a
  // 2. iff a < b, then indexOf(a) < indexOf(b)
  def indexOf(a: A): UBitInt[W]
  def apply(index: UBitInt[W]): A
  
  def minIdx: UBitInt[W] = indexOf(bounded.minBound)
  def maxIdx: UBitInt[W] = indexOf(bounded.maxBound)
  def size: SafeLong = maxIdx.toSafeLong - minIdx.toSafeLong

}
