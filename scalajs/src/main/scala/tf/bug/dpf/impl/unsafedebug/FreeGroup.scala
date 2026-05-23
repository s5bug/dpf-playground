package tf.bug.dpf.impl.unsafedebug

import cats.effect.{Concurrent, Ref}
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.mutable
import scodec.Codec
import scodec.bits.BitVector
import spire.algebra.Group
import tf.bug.dpf.impl.Advised
import tf.bug.dpf.*
import tf.bug.{BitVecN, UBitInt}

final case class FreeGroupElement[A](symbol: A, negative: Boolean)

final case class FreeGroup[A](components: Vector[FreeGroupElement[A]]) {

  override def toString: String =
    components.map { e =>
      if e.negative then s"(-${e.symbol})"
      else e.symbol.toString
    }.mkString(" + ")

}

object FreeGroup {

  def apply[A](single: FreeGroupElement[A]): FreeGroup[A] = FreeGroup(Vector(single))

  def uniquePositiveSampler[F[_], S, A](first: S, next: S => (S, A))(using conc: Concurrent[F]): F[Sampler[F, Advised[FreeGroup[A]]]] =
    Ref.of[F, S](first).map { nextRef =>
      Sampler(nextRef.modify(next).map(FreeGroupElement(_, false)).map(FreeGroup(_)).map(Advised(_, false, false)))
    }

  def fixBase26(original: String): String = original.map {
    case c if c >= '0' && c <= '9' => (c - '0' + 'a').toChar
    case c if c >= 'a' && c <= 'p' => (c - 'a' + 'k').toChar
    case c => throw new IllegalArgumentException(s"unrecognized char ${c} in base26 adjustment")
  }

  def sampleStringSymbols[F[_]](using conc: Concurrent[F]): F[Sampler[F, Advised[FreeGroup[String]]]] =
    uniquePositiveSampler[F, Int, String](0, current => {
      val base26 = Integer.toString(current, 26)
      val out = fixBase26(base26)
      (current + 1, out)
    })

  given groupForFreeGroup[A]: Group[FreeGroup[A]] with {
    def empty: FreeGroup[A] = FreeGroup(Vector.empty)

    @tailrec
    final def combine(x: FreeGroup[A], y: FreeGroup[A]): FreeGroup[A] =
      if x.components.isEmpty then y
      else if y.components.isEmpty then x
      else if x.components.last.symbol == y.components.head.symbol &&
        x.components.last.negative != y.components.head.negative then combine(FreeGroup(x.components.init), FreeGroup(y.components.tail))
      else FreeGroup(x.components ++ y.components)

    def inverse(a: FreeGroup[A]): FreeGroup[A] =
      FreeGroup(a.components.reverseIterator.map(e => e.copy(negative = !e.negative)).toVector)
  }

  final class StringSymbolPrg(var ctr: Int, seed: Int) extends Seeding[Advised[FreeGroup[String]]] {
    override val seedIsCorrectable: Correctable[Advised[FreeGroup[String]]] = summon

    private val rnd: scala.util.Random = scala.util.Random.javaRandomToRandom(new java.util.Random(seed))

    private val cache: mutable.Map[Advised[FreeGroup[String]], (Advised[FreeGroup[String]], Advised[FreeGroup[String]])] =
      mutable.Map()

    override def expand(root: Advised[FreeGroup[String]]): (Advised[FreeGroup[String]], Advised[FreeGroup[String]]) =
      cache.getOrElseUpdate(root, {
        println("prg: called with new " + root)
        if root.party0 || root.party1 then System.err.println("prg called with high advice, this should never happen!")

        val leftSymbol = FreeGroupElement[String](fixBase26(Integer.toString(ctr, 26)), false)
        ctr += 1
        val rightSymbol = FreeGroupElement[String](fixBase26(Integer.toString(ctr, 26)), false)
        ctr += 1

        val left = Advised(root.element |+| FreeGroup(leftSymbol), rnd.nextBoolean(), rnd.nextBoolean())
        val right = Advised(root.element |+| FreeGroup(rightSymbol), rnd.nextBoolean(), rnd.nextBoolean())
        (left, right)
      })
  }

  def unsafeMemoryCodec(): Codec[Advised[FreeGroup[String]]] = {
    val myCache: mutable.ArrayBuffer[Advised[FreeGroup[String]]] = mutable.ArrayBuffer()

    scodec.codecs.uint8.xmap(myCache(_), theValue => {
      val existingIdx = myCache.indexOf(theValue)
      if existingIdx != -1 then existingIdx
      else {
        myCache.addOne(theValue)
        myCache.size - 1
      }
    })
  }

  final class ClearEmbedding[X, A, Y](val xDomain: Domain[X], val yDomain: Domain[Y], val yIsGroup: Group[Y], val seed: Int) extends Embedding[X, Advised[FreeGroup[A]], Y] {
    val xCodec: Codec[UBitInt[xDomain.W]] = UBitInt.codec[xDomain.W](using xDomain.bitwidth)

    def xToBits(x: X): BitVector =
      xCodec.encode(xDomain.indexOf(x)).toTry.get

    override def directions(x: X): IterableOnce[TreeDirection] = {
      val xBits = xToBits(x)
      (0 until xBits.size.toInt).map(idx => if xBits(idx) then TreeDirection.Right else TreeDirection.Left)
    }

    private val rnd: scala.util.Random = scala.util.Random.javaRandomToRandom(new java.util.Random(seed))

    private val cache: mutable.Map[(X, Advised[FreeGroup[A]]), Y] =
      mutable.Map()

    override def extract(at: X, from: Advised[FreeGroup[A]]): Y = cache.getOrElseUpdate((at, from), {
      println(s"embedding: generating at ${at} from ${from}")

      // generate a random index with 4x the bit width for better randomness
      val quadWidth = yDomain.bitwidth.value * 4
      val bytes = rnd.nextBytes((quadWidth + 7) / 8)
      val bitVec = BitVector.view(bytes, quadWidth)
      val bvN: BitVecN[quadWidth.type] = BitVecN(bitVec)
      val number: UBitInt[quadWidth.type] = bvN.toUBitInt
      val max: UBitInt[quadWidth.type] = yDomain.indexOf(yDomain.bounded.maxBound).asInstanceOf[UBitInt[quadWidth.type]]
      val min: UBitInt[quadWidth.type] = yDomain.indexOf(yDomain.bounded.minBound).asInstanceOf[UBitInt[quadWidth.type]]

      val idx = min + (number % (max - min + UBitInt(1)))
      val idxAs: UBitInt[yDomain.W] = idx.asInstanceOf[UBitInt[yDomain.W]]

      yDomain(idxAs)
    })

  }

}
