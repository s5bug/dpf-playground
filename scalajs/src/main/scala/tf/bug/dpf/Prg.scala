package tf.bug.dpf

abstract class Prg[I, O] extends ((I, Int) => Vector[O]) {

  override def apply(seed: I, numOutputs: Int): Vector[O]
  
}
