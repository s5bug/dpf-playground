package tf.bug.dpf.impl

import cats.Monad
import cats.effect.std.Random
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import scodec.Codec
import spire.algebra.Group
import tf.bug.dpf.{Correctable}

final case class Advised[A](element: A, party0: Boolean, party1: Boolean)

object Advised {
  
  def codec[A](aCodec: Codec[A]): Codec[Advised[A]] = {
    import scodec.codecs.*
    (aCodec :: bool :: bool).as[Advised[A]]
  }
  
  def insecureEmptyOf[A](a: A): Advised[A] = Advised(a, false, false)

  given groupForAdvised[A](using evA: Group[A]): Group[Advised[A]] with {
    def inverse(a: Advised[A]): Advised[A] =
      Advised(evA.inverse(a.element), a.party0, a.party1)

    val empty: Advised[A] =
      Advised(evA.empty, false, false)
      
    def combine(x: Advised[A], y: Advised[A]): Advised[A] =
      Advised(evA.combine(x.element, y.element), x.party0 != y.party0, x.party1 != y.party1)
  }
  
  given correctibleForAdvised[A](using evA: Group[A]): Correctable[Advised[A]] with {
    // lsb (party 1) is always taken as parity
    override def getParity(a: Advised[A]): Boolean = a.party1
    override def storeParity(into: Advised[A], party1: Boolean): Advised[A] =
      Advised(into.element, into.party0, party1)

    def getAdvice(a: Advised[A]): (Boolean, Boolean) =
      (a.party0, a.party1)
    def storeAdvice(into: Advised[A], party0: Boolean, party1: Boolean): Advised[A] =
      Advised(into.element, party0, party1)
  }

}
