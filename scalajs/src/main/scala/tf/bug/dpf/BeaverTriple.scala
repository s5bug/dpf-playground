package tf.bug.dpf

import cats.{Applicative, Monad}
import cats.effect.std.Random
import spire.algebra.{Group, Ring}

final case class BeaverTriple[A](a: A, b: A, c: A) {
  def share[F[_]](sampler: Sampler[F, A])(using group: Group[A], shareThrice: Applicative[F]): F[(BeaverTriple[A], BeaverTriple[A])] = {
    val aShares = Sharing.forGroupUniform(sampler, a, 2)
    val bShares = Sharing.forGroupUniform(sampler, b, 2)
    val cShares = Sharing.forGroupUniform(sampler, c, 2)
    shareThrice.map3(aShares, bShares, cShares) {
      case (Vector(a0, a1), Vector(b0, b1), Vector(c0, c1)) =>
        (BeaverTriple(a0, b0, c0), BeaverTriple(a1, b1, c1))
    }
  }
}

object BeaverTriple {
  def fromCoefficients[F[_], A](sampler: Sampler[F, A])(using r: Ring[A], sampleTwice: Applicative[F]): Sampler[F, BeaverTriple[A]] =
    Sampler(sampleTwice.map2(sampler.uniform, sampler.uniform) { (a, b) =>
      val c = r.times(a, b)
      BeaverTriple(a, b, c)
    })
  
  given sampleableUnshared[A](using r: Ring[A], sampleable: Sampleable[A]): Sampleable[BeaverTriple[A]] with {
    override def apply[F[_]](random: Random[F])(using monad: Monad[F]): Sampler[F, BeaverTriple[A]] =
      fromCoefficients[F, A](sampleable(random))
  }
}
