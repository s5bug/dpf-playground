package tf.bug.dpf

import scodec.bits.BitVector
import spire.algebra.Group

trait Correctable[A] {

  // things that are correctable form a group
  given group: Group[A] = compiletime.deferred

  // getAdvice and storeAdvice must be group homomorphisms
  // that means
  // 1. g(a) ^ g(b) = g(a + b)
  // 2. s(a, b, c) + s(d, e, f) = s(a + d, b ^ e, c ^ f)
  // if these are true, it follows that:
  // a. g(0) = 0
  // b. s(0, false, false) = 0
  // c. g(-a) = -g(a) =«because of Booleans»= g(a)
  // d. s(a, b, c) = -s(-a, -b, -c) =«because of Booleans»= -s(-a, b, c) = s(a, -b, -c)
  // this is true in general when A is X × Bool × Bool for any group X
  // it is NOT true when A is ℤₙ
  def getAdvice(a: A): (Boolean, Boolean)
  def storeAdvice(into: A, party0: Boolean, party1: Boolean): A

  def getParity(a: A): Boolean = this.getAdvice(a)._2
  def storeParity(into: A, party1: Boolean): A = this.storeAdvice(into, this.getAdvice(into)._1, party1)

}

object Correctable {

  inline def apply[A](using ev: Correctable[A]): ev.type = ev

}
