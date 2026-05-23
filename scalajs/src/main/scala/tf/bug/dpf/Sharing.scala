package tf.bug.dpf

import cats.Applicative
import cats.effect.std.Random
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.group.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import spire.algebra.Group

object Sharing {
  
  def forGroupUniform[F[_], A](random: Sampler[F, A], secret: A, parties: Int)(using Applicative[F], Group[A]): F[Vector[A]] = {
    val left = (1 until parties).toVector.traverse { _idx => random.uniform }
    left.map { (v: Vector[A]) =>
      val sum = v.combineAll
      val difference = sum.inverse() |+| secret
      v.appended(difference)
    }
  }
  
}
