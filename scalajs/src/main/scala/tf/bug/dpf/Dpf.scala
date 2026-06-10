package tf.bug.dpf

import cats.{Applicative, Monad}
import cats.effect.std.Random
import cats.syntax.all.*
import scodec.bits.BitVector
import spire.algebra.Group
import tf.bug.dpf.Party.Party1
import tf.bug.{UBitInt, dpf}

final case class Dpf[X, S, L, Y](seed0: S, seed1: S, cws0: Vector[S], cws1: Vector[S], advices: BitVector, leaf0: L, leaf1: L) {

  lazy val p0half: Dpf.Half[X, S, L, Y] = Dpf.Half(seed0, cws0, advices, leaf0)
  lazy val p1half: Dpf.Half[X, S, L, Y] = Dpf.Half(seed1, cws1, advices, leaf1)
  
  // produces a corrected leaf value for a certain seed, party 0
  def eval0(x: X, embedding: Embedding.Aux[X, S, L, Y], seeding: Seeding[S]): Y = {
    p0half.eval(Party.Party0, x, embedding, seeding)
  }

  // produces a corrected leaf value for a certain seed, party 1
  def eval1(x: X, embedding: Embedding.Aux[X, S, L, Y], seeding: Seeding[S]): Y = {
    p1half.eval(Party.Party1, x, embedding, seeding)
  }

  def apply(x: X, embedding: Embedding.Aux[X, S, L, Y], seeding: Seeding[S]): Y = {
    val p0 = eval0(x, embedding, seeding)
    val p1 = eval1(x, embedding, seeding)

    println("p0's share: " + p0)
    println("p1's share: " + p1)

    embedding.yIsGroup.combine(p1, embedding.yIsGroup.inverse(p0))
  }

}

object Dpf {

  final case class Half[X, S, L, Y](seed: S, cws: Vector[S], advices: BitVector, leaf: L) {
    
    def eval(party: Party, at: X, embedding: Embedding.Aux[X, S, L, Y], seeding: Seeding[S]): Y = {
      // TODO replace printlns with modifying some log for use in displaying
      println("!!! EVAL: " + party)

      var parent: S = seed

      embedding.directions(at).iterator.zipWithIndex.foreach { (direction, idx) =>
        val myAdvice = seeding.seedIsCorrectable.getParity(parent)

        println("parent: " + parent)
        println("direction: " + direction)
        println("myAdvice: " + myAdvice)

        val (childL, childR) = seeding.expand(seeding.seedIsCorrectable.storeAdvice(parent, false, false))

        println("childL: " + childL)
        println("childR: " + childR)

        // the correction word is cws(idx) with the lowest bit set to
        // - advices(2*idx) if going right,
        // - advices(1 + 2*idx) if going left
        val rawCw = cws(idx)
        val rightAdvice = advices(idx << 1)
        val leftAdvice = advices(1 | (idx << 1))
        val cw = direction match {
          case TreeDirection.Left => seeding.seedIsCorrectable.storeParity(rawCw, leftAdvice)
          case TreeDirection.Right => seeding.seedIsCorrectable.storeParity(rawCw, rightAdvice)
        }

        println("rawCw: " + rawCw)
        println("leftAdvice: " + leftAdvice)
        println("rightAdvice: " + rightAdvice)
        println("cw: " + cw)

        // if myAdvice,
        // - party 0 reconstructs with Node + CW
        // - party 1 reconstructs with Node - CW
        parent = direction match {
          case TreeDirection.Left => childL
          case TreeDirection.Right => childR
        }
        if myAdvice then {
          val op = party match {
            case Party.Party0 => cw
            case Party.Party1 => seeding.seedIsCorrectable.group.inverse(cw)
          }
          parent = seeding.seedIsCorrectable.group.combine(parent, op)
        }
      }

      val myAdvice = seeding.seedIsCorrectable.getParity(parent)
      parent = seeding.seedIsCorrectable.storeAdvice(parent, false, false)
      println("final leaf node: " + parent)

      // parent is now our leaf seed
      val leafL = embedding.lengthen(parent)
      // if parent has the final bit set, we apply the correction word
      // p0 takes leafy + CW, p1 takes leafy - CW
      val result = if myAdvice then {
        val op = party match {
          case Party.Party0 => leaf
          case Party.Party1 => embedding.lIsGroup.inverse(leaf)
        }
        embedding.lIsGroup.combine(leafL, op)
      } else leafL

      embedding.extract(at, result)
    }

  }

  final case class Prepared[S](seed0: S, seed1: S)
  
  def prepare[F[_], S](sampleSeed: Sampler[F, S])(using app: Applicative[F]): F[Prepared[S]] =
    (sampleSeed.uniform, sampleSeed.uniform).mapN(Prepared[S])
  
  def prepare[F[_], S](random: Random[F])(using mnd: Monad[F], evS: Sampleable[S]): F[Prepared[S]] =
    prepare(evS[F](random))
  
  def generate[X, S, L, Y](
    prepared: Prepared[S],
    input: X, output: Y,
    embedding: Embedding.Aux[X, S, L, Y],
    seeding: Seeding[S],
  ): Dpf[X, S, L, Y] = {
    val Prepared(seed0, seed1) = prepared
    
    given groupS: Group[S] = seeding.seedIsCorrectable.group

    val initialCw = groupS.empty

    // left party should have low bit unset
    val seed0c = seeding.seedIsCorrectable.storeParity(seed0, false)

    // right party should have low bit set
    val seed1c = seeding.seedIsCorrectable.storeParity(seed1, true)

    // we can use the fact that most of the tree is zero to only ever work with one parent per party at a time
    var parent0 = seed0c
    var parent1 = seed1c

    // and we have a correction word for each level
    // the party that is not "supposed" to apply the correction words gets the negated versions
    val cws0Builder = Vector.newBuilder[S]
    val cws1Builder = Vector.newBuilder[S]
    val advicesBuilder = Vector.newBuilder[Boolean]

    embedding.directions(input).iterator.foreach { direction =>
      println("direction: " + direction)

      // we always want to invoke the PRNG with the advice bits set low
      println("parent0: " + parent0)
      val advice0 = seeding.seedIsCorrectable.getParity(parent0)
      println("advice0: " + advice0)
      parent0 = seeding.seedIsCorrectable.storeAdvice(parent0, false, false)

      println("parent1: " + parent1)
      val advice1 = seeding.seedIsCorrectable.getParity(parent1)
      println("advice1: " + advice1)
      parent1 = seeding.seedIsCorrectable.storeAdvice(parent1, false, false)

      val (child0L, child0R) = seeding.expand(parent0)
      val (child1L, child1R) = seeding.expand(parent1)

      println("child0L: " + child0L + " child0R: " + child0R)
      println("child1L: " + child1L + " child1R: " + child1R)

      // we consider the DPF to be subtractive shared
      // that is, if Alice holds X, and Bob holds Y as DPF parts
      // we construct the correction word as (-X) + Y
      // Alice reconstructs with X + ((-X) + Y) = X - X + Y
      // Bob reconstructs with Y - ((-X) + Y) = Y - Y + X
      val childL = groupS.combine(groupS.inverse(child0L), child1L)
      val childR = groupS.combine(groupS.inverse(child0R), child1R)

      println("childL: " + childL)
      println("childR: " + childR)

      // because we stored advice, we have an LSB mismatch
      // we use the direction we're _not_ going as the correction word
      // so if we're going right, the correction word comes from the left
      // if we're going left, the correction word comes from the right

      // the overall parity of each child encodes whether we should go that direction
      // that is, "the direction we don't want to go" has a cleared LSB
      val tLeft = direction match {
        case TreeDirection.Left => !seeding.seedIsCorrectable.getParity(childL)
        case TreeDirection.Right => seeding.seedIsCorrectable.getParity(childL)
      }
      val tRight = direction match {
        case TreeDirection.Left => seeding.seedIsCorrectable.getParity(childR)
        case TreeDirection.Right => !seeding.seedIsCorrectable.getParity(childR)
      }

      direction match {
        case TreeDirection.Left =>
          // the correction word is childR, but with the LSB set to !lsb(childL)
          // this way we know that the correction word is only applied to one node (the one with the true lsb)
          val correctionWord = seeding.seedIsCorrectable.storeParity(childR, tLeft)

          // we're moving down the left of the tree
          // if p0 is applying the correction word, then they take X + CW
          parent0 = if advice0 then groupS.combine(child0L, correctionWord) else child0L
          // if p1 is applying the correction word, then they take Y - CW
          parent1 = if advice1 then groupS.combine(child1L, groupS.inverse(correctionWord)) else child1L

          // childR is the public CW
          // if a certain party is "supposed" to apply a CW along the correct path, negate the other's CW
          cws0Builder.addOne(if advice0 then childR else groupS.inverse(childR))
          cws1Builder.addOne(if advice1 then childR else groupS.inverse(childR))
        case TreeDirection.Right =>
          // the correction word is childL, but with the LSB set to !lsb(childR)
          // this way we know that the correction word is only applied to one node (the one with the true lsb)
          val correctionWord = seeding.seedIsCorrectable.storeParity(childL, tRight)

          // we're moving down the right of the tree
          // if p0 is applying the correction word, then they take X + CW
          parent0 = if advice0 then groupS.combine(child0R, correctionWord) else child0R
          // if p1 is applying the correction word, then they take Y - CW
          parent1 = if advice1 then groupS.combine(child1R, groupS.inverse(correctionWord)) else child1R

          // childL is the public CW
          // if a certain party is "supposed" to apply a CW along the correct path, negate the other's CW
          cws0Builder.addOne(if advice0 then childL else groupS.inverse(childL))
          cws1Builder.addOne(if advice1 then childL else groupS.inverse(childL))
      }

      advicesBuilder.addOne(tRight)
      advicesBuilder.addOne(tLeft)
    }

    // parent0 and parent1 are now our leaf nodes, so let's embed the values in them
    val sign = seeding.seedIsCorrectable.getParity(parent0)
    parent0 = seeding.seedIsCorrectable.storeAdvice(parent0, false, false)
    parent1 = seeding.seedIsCorrectable.storeAdvice(parent1, false, false)

    println("final leaf node p0: " + parent0)
    println("final leaf node p1: " + parent1)

    // when we evaluate,
    // p0 will calculate embedding.extract(x, parent0), + CW depending on sign
    // p1 will calculate embedding.extract(x, parent1), - CW depending on sign
    // in the non-target case, these will be equal to each-other
    // in the target case, let's call these p0y and p1y respectively
    // we want p1y - p0y = y
    // - in the sign = true case, p0 applies the CW: this results in p0y = extract(x, parent0) + CW
    // - in the sign = false case, p1 applies the CW: this results in p1y = extract(x, parent1) - CW
    // in the first case we end up with (extract(x, parent1) - CW) - extract(x, parent0) = y
    // in the second case we end up with extract(x, parent1) - (extract(x, parent0) + CW) = y
    // so we can do some rearrangement:
    // - in the first case, CW = -extract(x, parent0) + -y + extract(x, parent1)
    // - in the second case, CW = -extract(x, parent0) + -y + extract(x, parent1)

    val p0l = embedding.lengthen(parent0)
    val p1l = embedding.lengthen(parent1)
    
    val negativeP0l = embedding.lIsGroup.inverse(p0l)
    val outputL = embedding.embed(input, output)
    val negativeOutputL = embedding.lIsGroup.inverse(outputL)
    val leafCw = embedding.lIsGroup.combine(negativeP0l, embedding.lIsGroup.combine(negativeOutputL, p1l))

    // the party that's not supposed to apply the CW gets the negated version
    val p0cw = if sign then leafCw else embedding.lIsGroup.inverse(leafCw)
    val p1cw = if sign then embedding.lIsGroup.inverse(leafCw) else leafCw
    Dpf(seed0c, seed1c, cws0Builder.result(), cws1Builder.result(), BitVector.bits(advicesBuilder.result()), p0cw, p1cw)
  }

}
