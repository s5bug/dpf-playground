package tf.bug.dpfplayground.state

import cats.effect.{IO, Resource}
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement

abstract class SimpleInstanceOption {
  def name: String

  type Params
  def defaultParams: Option[Params] = None
  def takeParams(sref: SignallingRef[IO, Option[Params]]): Option[Resource[IO, HtmlElement[IO]]]

  type Instance[P <: Params & Singleton]
  def defaultInstance(params: Params): Option[Instance[params.type]] = None
  def takeInstance(params: Params, sref: SignallingRef[IO, Option[Instance[params.type]]]): Option[Resource[IO, HtmlElement[IO]]]
}

object SimpleInstanceOption {

  abstract class Companion[A <: SimpleInstanceOption] {
    def values: Vector[A]
    
    given eqForSimpleInstanceOption: cats.Eq[A] = cats.Eq.fromUniversalEquals
  }
  
  sealed abstract class ParamsPair[A <: SimpleInstanceOption] {
    val x: A
    val y: x.Params
  }
  object ParamsPair {
    def apply[A <: SimpleInstanceOption](x0: A, y0: x0.Params): ParamsPair[A] =
      new ParamsPair[A] {
        final val x: x0.type = x0
        final val y: y0.type = y0
      }
  }
  
  sealed abstract class Exists[A <: SimpleInstanceOption, F[_]] {
    val x: A
    val y: x.Params
    val z: F[x.Instance[y.type]]
  }
  object Exists {
    def apply[A <: SimpleInstanceOption, F[_]](x0: A, y0: x0.Params, z0: F[x0.Instance[y0.type]]): Exists[A, F] =
      new Exists[A, F] {
        final val x: x0.type = x0
        final val y: y0.type = y0
        final val z: z0.type = z0
      }
  }
  
  type ParamsInstanceTriple[A <: SimpleInstanceOption] = Exists[A, cats.Id]
  type ParamsInstanceTriple2[A <: SimpleInstanceOption] = Exists[A, [z] =>> (z, z)]

}
