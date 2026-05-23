package tf.bug.dpf

import cats.{Functor, Monad}
import cats.effect.std.Random
import cats.syntax.functor.*

opaque type Sampler[+F[_], A] = F[A]

object Sampler {
  
  inline def apply[F[_], A](fa: F[A]): Sampler[F, A] = fa
  
  extension[F[_], A](fa: Sampler[F, A]) {
    inline def uniform: F[A] = fa
  }
  
  // f must be bijective
  inline def ofSampleable[A](using sampleable: Sampleable[A])[F[_]](random: Random[F])(using monad: Monad[F]): Sampler[F, A] =
    sampleable(random)
  
}
