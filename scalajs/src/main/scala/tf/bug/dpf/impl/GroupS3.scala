package tf.bug.dpf.impl

import calico.html.Html
import cats.Monad
import cats.effect.{Async, Resource}
import cats.effect.std.Random
import cats.kernel.{BoundedEnumerable, Order}
import fs2.concurrent.Signal
import fs2.dom.HtmlElement
import scodec.Codec
import spire.algebra.Group
import tf.bug.dpf.Domain
import tf.bug.dpf.impl.GroupS3.unsafeGroupS3IsDomain

enum GroupS3 extends java.lang.Enum[GroupS3] {
  case Identity
  case Swap12
  case Swap13
  case Swap23
  case CycleUp
  case CycleDown
}

object GroupS3 {

  def codec: Codec[GroupS3] = {
    import scodec.codecs.*
    uint(3).xmap(GroupS3.fromOrdinal, _.ordinal())
  }
  
  given groupS3IsGroup: Group[GroupS3] with {
    def empty: GroupS3 = GroupS3.Identity

    // composition is "do y, then do x"
    def combine(x: GroupS3, y: GroupS3): GroupS3 = x match {
      case GroupS3.Identity => y
      case GroupS3.Swap12 => y match {
        case GroupS3.Identity => x
        case GroupS3.Swap12 => GroupS3.Identity
        case GroupS3.Swap13 => GroupS3.CycleDown
        case GroupS3.Swap23 => GroupS3.CycleUp
        case GroupS3.CycleUp => GroupS3.Swap23
        case GroupS3.CycleDown => GroupS3.Swap13
      }
      case GroupS3.Swap13 => y match {
        case GroupS3.Identity => x
        case GroupS3.Swap12 => GroupS3.CycleUp
        case GroupS3.Swap13 => GroupS3.Identity
        case GroupS3.Swap23 => GroupS3.CycleDown
        case GroupS3.CycleUp => GroupS3.Swap12
        case GroupS3.CycleDown => GroupS3.Swap23
      }
      case GroupS3.Swap23 => y match {
        case GroupS3.Identity => x
        case GroupS3.Swap12 => GroupS3.CycleDown
        case GroupS3.Swap13 => GroupS3.CycleUp
        case GroupS3.Swap23 => GroupS3.Identity
        case GroupS3.CycleUp => GroupS3.Swap13
        case GroupS3.CycleDown => GroupS3.Swap12
      }
      case GroupS3.CycleUp => y match {
        case GroupS3.Identity => x
        case GroupS3.Swap12 => GroupS3.Swap13
        case GroupS3.Swap13 => GroupS3.Swap23
        case GroupS3.Swap23 => GroupS3.Swap12
        case GroupS3.CycleUp => GroupS3.CycleDown
        case GroupS3.CycleDown => GroupS3.Identity
      }
      case GroupS3.CycleDown => y match {
        case GroupS3.Identity => x
        case GroupS3.Swap12 => GroupS3.Swap23
        case GroupS3.Swap13 => GroupS3.Swap12
        case GroupS3.Swap23 => GroupS3.Swap13
        case GroupS3.CycleUp => GroupS3.Identity
        case GroupS3.CycleDown => GroupS3.CycleUp
      }
    }

    override def inverse(a: GroupS3): GroupS3 = a match {
      case GroupS3.Identity => GroupS3.Identity
      case GroupS3.Swap12 => GroupS3.Swap12
      case GroupS3.Swap13 => GroupS3.Swap13
      case GroupS3.Swap23 => GroupS3.Swap23
      case GroupS3.CycleUp => GroupS3.CycleDown
      case GroupS3.CycleDown => GroupS3.CycleUp
    }
  }

  object unsafeGroupS3IsBounded extends BoundedEnumerable[GroupS3] {
    override def order: Order[GroupS3] = Order.by(_.ordinal())
    override def minBound: GroupS3 = GroupS3.values.head
    override def partialNext(a: GroupS3): Option[GroupS3] = util.Try(GroupS3.fromOrdinal(a.ordinal() + 1)).toOption
    override def partialPrevious(a: GroupS3): Option[GroupS3] = util.Try(GroupS3.fromOrdinal(a.ordinal() - 1)).toOption
    override def maxBound: GroupS3 = GroupS3.values.last
  }

  object unsafeGroupS3IsDomain extends Domain[GroupS3] {
    override given bounded: BoundedEnumerable[GroupS3] = unsafeGroupS3IsBounded

    override type W = 3
    override def indexOf(a: GroupS3): UBitInt[W] = UBitInt[W](a.ordinal())
    override def apply(index: UBitInt[W]): GroupS3 = GroupS3.fromOrdinal(index.toInt)
  }

}
