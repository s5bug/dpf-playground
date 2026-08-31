package tf.bug.dsgrotto

import tf.bug.dpf.Domain
import tf.bug.dpf.impl.{BitVecN, UBitInt}

object DsGrotto {

  // at the end of the prefix party algorithm, each party should end up with a XOR share of a point vector
  // this point vector corresponds to what interval the input point is in
  // so we have a plaintext list of endpoints and a secret-shared input point
  // TODO this should be converted to a choreography
  // probably the primitives that are needed during an exchange get wrapped up into a tagless thing
  // p0 gets one instance, p1 gets the other
  def grottoPrefixParity[X, W <: Int](
    endpoints: Vector[X],
    p0ShareOfIndex: UBitInt[W],
    p1ShareOfIndex: UBitInt[W],
  )(using domain: Domain[X]): Int = {
    val sortedEndpoints = endpoints.sorted(using Domain.orderingForDomain)

    // we only care about the CWs for the parts of the tree that can be traversed by `endpoints`
    
    ???
  }

}
