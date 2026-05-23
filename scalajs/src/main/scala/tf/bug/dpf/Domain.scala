package tf.bug.dpf

import cats.kernel.BoundedEnumerable
import tf.bug.UBitInt

trait Domain[A] {

  type W <: Int
  given bitwidth: ValueOf[W] = compiletime.deferred

  given bounded: BoundedEnumerable[A] = compiletime.deferred

  // indexOf and apply must be strictly monotonic
  // i.e. [f(x) < f(y)] iff [x < y]
  def indexOf(a: A): UBitInt[W]
  def apply(index: UBitInt[W]): A

}
