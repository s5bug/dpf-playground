package tf.bug

import calico.html.{Html, HtmlAttr}
import cats.syntax.functor.given
import cats.{Monad, Show}
import cats.effect.std.Random
import cats.effect.{Async, Resource}
import cats.kernel.BoundedEnumerable
import fs2.concurrent.Signal
import fs2.dom.HtmlElement
import scala.collection.IterableOnce
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import spire.algebra.{Group, Order}
import spire.math.SafeLong
import tf.bug
import tf.bug.dpf.{Correctable, Domain}

opaque type UBitInt[N <: Int] = SafeLong

object UBitInt {
  
  def codec[N <: Int](using n: ValueOf[N]): Codec[UBitInt[N]] =
    BitVecN.codec[N].xmap(_.toUBitInt, _.toBitVecN)

  extension[N <: Int](u: UBitInt[N]) {
    def +(other: UBitInt[N])(using n: ValueOf[N]): UBitInt[N] = {
      // naive addition
      val big = u + other
      val reduced = big % (SafeLong.one << n.value)
      reduced
    }

    def -(other: UBitInt[N])(using n: ValueOf[N]): UBitInt[N] =
      UBitInt.+(u)((-UBitInt)(other))

    def unary_-(using n: ValueOf[N]): UBitInt[N] = {
      val big = -u
      val corrected = if big < 0 then big + (SafeLong.one << n.value) else big
      corrected
    }

    def &(other: UBitInt[N])(using n: ValueOf[N]): UBitInt[N] =
      (u: SafeLong) & (other: SafeLong)

    def ^(other: UBitInt[N])(using n: ValueOf[N]): UBitInt[N] =
      (u: SafeLong) ^ (other: SafeLong)

    def %(other: UBitInt[N])(using n: ValueOf[N]): UBitInt[N] =
      (u: SafeLong) % (other: SafeLong)

    def toInt: Int =
      if u > Int.MaxValue then throw new IllegalArgumentException(s"${u} too big for Int")
      else u.toInt
      
    def toBitVecN(using n: ValueOf[N]): BitVecN[N] =
      BitVector.fromBigInt(u.toBigInt, Some(n.value)).asInstanceOf[BitVecN[N]]
  }

  def apply[N <: Int](value: BigInt)(using n: ValueOf[N]): UBitInt[N] = {
    val modulus = SafeLong.one << n.value
    val reduced = ((SafeLong(value)  %  modulus) + modulus) % modulus
    reduced
  }

  given ubitIntGroup[N <: Int](using n: ValueOf[N]): Group[UBitInt[N]] with {
    def combine(x: UBitInt[N], y: UBitInt[N]): UBitInt[N] = UBitInt.+(x)(y)
    def inverse(x: UBitInt[N]): UBitInt[N] = (-UBitInt)(x)
    def empty: UBitInt[N] = SafeLong.zero
  }

  private final val subscriptDigits = "₀₁₂₃₄₅₆₇₈₉"
  given ubitIntShow[N <: Int](using n: ValueOf[N]): Show[UBitInt[N]] with {
    val subscriptValue: String = "₂‸" ++ n.value.toString.map(_ - '0').map(subscriptDigits(_)).mkString
    def show(t: UBitInt[N]): String = s"${t} ∈ ℕ${subscriptValue}"
  }

  given ubitIntHtmlShow[N <: Int](using n: ValueOf[N]): HtmlShow[UBitInt[N]] with {
    def html[F[_]](a: Signal[F, UBitInt[N]])(using async: Async[F]): Resource[F, HtmlElement[F]] = {
      import calico.frp.given
      val html = Html[F]
      import html.given
      component.Katex(
        html.dataAttr("src") <-- a.map(b => raw"""${b} \in \mathbb{N}_{2^{${n.value}}}""")
      )
    }
  }

  given ubitIntOrder[N <: Int](using n: ValueOf[N]): Order[UBitInt[N]] with {
    def compare(x: UBitInt[N], y: UBitInt[N]): Int = Order[SafeLong].compare(x, y)
  }

  given ubitIntBoundedEnumerable[N <: Int](using n: ValueOf[N]): BoundedEnumerable[UBitInt[N]] with {
    val order: Order[UBitInt[N]] = ubitIntOrder[N]
    val minBound: UBitInt[N] = SafeLong.zero
    val maxBound: UBitInt[N] = (SafeLong.one << n.value) - 1

    def partialNext(a: UBitInt[N]): Option[UBitInt[N]] =
      if a >= maxBound then None
      else Some(a + 1)
    def partialPrevious(a: UBitInt[N]): Option[UBitInt[N]] =
      if a <= minBound then None
      else Some(a - 1)
  }

  given ubitIntDomain[N <: Int](using n: ValueOf[N]): Domain[UBitInt[N]] with {
    type W = N

    def indexOf(a: UBitInt[N]): UBitInt[N] = a
    def apply(index: UBitInt[N]): UBitInt[N] = index
  }
}

opaque type BitInt[N <: Int] = UBitInt[N]

object BitInt {
  
  inline def codec[N <: Int](using n: ValueOf[N]): Codec[BitInt[N]] =
    UBitInt.codec[N]

  extension[N <: Int] (bi: BitInt[N]) {
    def +(other: BitInt[N])(using value: ValueOf[N]): BitInt[N] = (bi: UBitInt[N]) + (other: UBitInt[N])
    def unary_-(using value: ValueOf[N]): BitInt[N] = -(bi: UBitInt[N])
  }

  def apply[N <: Int](value: BigInt)(using n: ValueOf[N]): BitInt[N] = UBitInt.apply[N](value)

  given bitIntGroup[N <: Int](using n: ValueOf[N]): Group[BitInt[N]] = UBitInt.ubitIntGroup[N]

  private final val subscriptDigits = "₀₁₂₃₄₅₆₇₈₉"
  given bitIntShow[N <: Int](using n: ValueOf[N]): Show[BitInt[N]] with {
    val overflowPoint: SafeLong = SafeLong.one << (n.value - 1)
    val size: SafeLong = overflowPoint << 1

    val subscriptValue: String = "₂‸" ++ n.value.toString.map(_ - '0').map(subscriptDigits(_)).mkString
    def show(t: BitInt[N]): String = {
      val isNegative = t > overflowPoint
      val corrected = if isNegative then t - size else t
      s"${corrected} ∈ ℤ${subscriptValue}"
    }
  }

  given bitIntHtmlShow[N <: Int](using n: ValueOf[N]): HtmlShow[BitInt[N]] with {
    val overflowPoint: SafeLong = SafeLong.one << (n.value - 1)
    val size: SafeLong = overflowPoint << 1

    def html[F[_]](a: Signal[F, BitInt[N]])(using async: Async[F]): Resource[F, HtmlElement[F]] = {
      import calico.frp.given
      val corrected = a.map { bi =>
        val isNegative = bi > overflowPoint
        if isNegative then bi - size else bi
      }
      val html = Html[F]
      import html.given
      component.Katex(
        html.dataAttr("src") <-- corrected.map(c => raw"""$c \in \mathbb{Z}_{2^{${n.value}}}""")
      )
    }
  }

  given bitIntOrder[N <: Int](using n: ValueOf[N]): Order[BitInt[N]] with {
    val overflowPoint: SafeLong = SafeLong.one << (n.value - 1)
    val size: SafeLong = overflowPoint << 1

    def compare(x: BitInt[N], y: BitInt[N]): Int = {
      val isXNegative = x > overflowPoint
      val xCorrected = if isXNegative then x - size else x
      val isYNegative = y > overflowPoint
      val yCorrected = if isYNegative then y - size else y
      Order[SafeLong].compare(xCorrected, yCorrected)
    }
  }

  given bitIntBoundedEnumerable[N <: Int](using n: ValueOf[N]): BoundedEnumerable[BitInt[N]] with {
    def order: Order[BitInt[N]] = bitIntOrder[N]

    val minBound: BitInt[N] = SafeLong.one << (n.value - 1)
    val maxBound: BitInt[N] = (SafeLong.one << (n.value - 1)) - 1

    def partialNext(a: BitInt[N]): Option[BitInt[N]] =
      if a == maxBound then None
      else if a == -(SafeLong.one: BitInt[N]) then Some(SafeLong.zero)
      else Some(a + 1)

    def partialPrevious(a: BitInt[N]): Option[BitInt[N]] =
      if a == minBound then None
      else if a == 0 then Some(-(SafeLong.one: BitInt[N]))
      else Some(a - 1)
  }

  given bitIntDomain[N <: Int](using n: ValueOf[N]): Domain[BitInt[N]] with {
    type W = N

    val msb: SafeLong = SafeLong.one << (n.value - 1)

    def indexOf(a: BitInt[N]): UBitInt[N] = a ^ msb
    def apply(index: UBitInt[N]): BitInt[N] = index ^ msb
  }
}
