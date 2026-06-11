package tf.bug

import calico.html.Html
import cats.Monad
import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.Random
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.dom.HtmlElement
import scodec.Codec
import scodec.bits.BitVector
import spire.algebra.Group
import spire.math.SafeLong
import tf.bug.dpf.{Correctable, Sampleable, Sampler}

opaque type BitVecN[N <: Int] = BitVector

object BitVecN {

  def codec[N <: Int](using n: ValueOf[N]): Codec[BitVecN[N]] = {
    import scodec.codecs.*
    bitsStrict(n.value)
  }

  extension[N <: Int](bvn: BitVecN[N]) {
    def toUBitInt: UBitInt[N] =
      SafeLong(bvn.toBigInt(false)).asInstanceOf[UBitInt[N]]
      
    def toBitInt: BitInt[N] =
      toUBitInt.asSigned

    def xor(other: BitVecN[N]): BitVecN[N] =
      bvn.xor(other)

    def raw: BitVector = bvn
  }

  def apply[N <: Int](bv: BitVector)(using v: ValueOf[N]): BitVecN[N] = {
    if bv.size != v.value then throw new IllegalArgumentException(s"expected BitVector of size ${v.value}, got ${bv.size}")

    bv
  }

  given bitVecNGroup[N <: Int](using v: ValueOf[N]): Group[BitVecN[N]] with {
    def empty: BitVecN[N] = BitVector.low(v.value)
    def combine(x: BitVecN[N], y: BitVecN[N]): BitVecN[N] = x.xor(y)
    def inverse(a: BitVecN[N]): BitVecN[N] = a
  }

  given bitVecNCorrectable[N <: Int](using v: ValueOf[N]): Correctable[BitVecN[N]] with {
    override def getParity(a: BitVecN[N]): Boolean = a(a.size - 1)
    override def storeParity(a: BitVecN[N], b: Boolean): BitVecN[N] = a.update(a.size - 1, b)

    def getAdvice(a: BitVecN[N]): (Boolean, Boolean) = (a(a.size - 2), a(a.size - 1))
    def storeAdvice(a: BitVecN[N], party0: Boolean, party1: Boolean): BitVecN[N] =
      a.update(a.size - 2, party0)
        .update(a.size - 1, party1)
  }

  given bitVecNHtmlShow[N <: Int]: HtmlShow[BitVecN[N]] with {
    def html[F[_]](a: Signal[F, BitVecN[N]])(using async: Async[F]): Resource[F, HtmlElement[F]] = {
      import calico.frp.given
      val html = Html[F]
      import html.given
      component.Katex(
        html.dataAttr("src") <-- a.map { b =>
          raw"""${b.toBin}_2"""
        }
      )
    }
  }
  
  given bitVecNSampleable[N <: Int](using n: ValueOf[N]): Sampleable[BitVecN[N]] with {
    def apply[F[_]](random: Random[F])(using monad: Monad[F]): Sampler[F, BitVecN[N]] = {
      val byteCount = (n.value + 7) / 8
      val fa = random.nextBytes(byteCount).map(BitVector.view(_, n.value))
      Sampler(fa)
    }
  }

}
