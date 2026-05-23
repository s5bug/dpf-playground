package tf.bug.dpf

abstract class Seeding[S] {

  val seedIsCorrectable: Correctable[S]
  def expand(root: S): (S, S)

}
