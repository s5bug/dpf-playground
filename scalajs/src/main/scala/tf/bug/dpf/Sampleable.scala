package tf.bug.dpf

import cats.Monad
import cats.effect.std.Random

trait Sampleable[A] {

  def apply[F[_]](random: Random[F])(using monad: Monad[F]): Sampler[F, A]
  
}
