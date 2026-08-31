package tf.bug.dpf

import cats.effect.std.Random
import cats.{Applicative, Functor, Monad}
import spire.algebra.{Group, Ring}
import tf.bug.dpf.impl.BitVecN

final case class Eda[X, W <: Int](arithmetic: X, boolean: BitVecN[W]) {
  def share[F[_]](arithSampler: Sampler[F, X], boolSampler: Sampler[F, BitVecN[W]])(using group: Group[X], w: ValueOf[W], shareTwice: Applicative[F]): F[(Eda[X, W], Eda[X, W])] = {
    val arithmeticShares = Sharing.forGroupUniform(arithSampler, arithmetic, 2)
    val booleanShares = Sharing.forGroupUniform(boolSampler, boolean, 2)
    shareTwice.map2(arithmeticShares, booleanShares) {
      case (Vector(a0, a1), Vector(b0, b1)) =>
        (Eda(a0, b0), Eda(a1, b1))
    }
  }
}

object Eda {
  def from[F[_], X](sampler: Sampler[F, X])(using d: Domain[X], sampleOnce: Functor[F]): Sampler[F, Eda[X, d.W]] =
    Sampler(sampleOnce.map(sampler.uniform) { element =>
      Eda(element, d.indexOf(element).toBitVecN)
    })

  given sampleableUnshared[X, W0 <: Int](using d: Domain[X] { type W = W0 }, sampleable: Sampleable[X]): Sampleable[Eda[X, W0]] with {
    override def apply[F[_]](random: Random[F])(using monad: Monad[F]): Sampler[F, Eda[X, d.W]] =
      from[F, X](sampleable(random))
  }
}
