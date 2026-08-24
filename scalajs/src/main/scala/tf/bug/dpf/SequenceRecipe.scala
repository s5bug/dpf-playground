package tf.bug.dpf

import scala.annotation.tailrec
import spire.math.SafeLong
import tf.bug.dpf.impl.UBitInt

final case class SequenceRecipe(
  steps: Vector[SequenceRecipe.Direction],
  // index of the recipe output leaf for each point
  leafIndices: Vector[Int],
  // number of leaves preserved in the recipe
  leafCount: Int,
  // steps.take(levelEndpoints(l)) gives you the steps required up to level l of the recipe
  levelEndpoints: Vector[Int],
)

object SequenceRecipe {
  
  enum Direction(weight: Int) extends java.lang.Enum[Direction] {
    case Left extends Direction(-1)
    case Both extends Direction(0)
    case Right extends Direction(1)
  }

  def make[X](endpoints: Vector[X], embedding: Embedding[?, ?])(using domain: Domain[X]): SequenceRecipe =
    unsafeMake(endpoints.sorted(using Domain.orderingForDomain), embedding)

  private final case class LevelState[W <: Int](
    currentSplits: Vector[Vector[UBitInt[W]]],
    recipeSteps: Vector[Direction],
    levelEndpoints: Vector[Int],
  )

  private final case class FoldState[W <: Int](
    nextSplits: Vector[Vector[UBitInt[W]]],
    nextSteps: Vector[Direction],
  )

  def unsafeMake[X](sortedEndpoints: Vector[X], embedding: Embedding[?, ?])(using domain: Domain[X]): SequenceRecipe = {
    // how many bits do we need to traverse to a leaf block if we were making a full DPF?
    val depth = embedding.treeDepth[X]
    // all the paths through the tree to the points we care about
    val indices = sortedEndpoints.map(domain.indexOf)

    val initialState = LevelState(
      // we have one split covering all indices
      currentSplits = Vector(indices),
      // we have yet to check direction at the root, so no steps yet
      recipeSteps = Vector.empty,
      // zero levels = take zero steps
      levelEndpoints = Vector(0),
    )

    val finalState: LevelState[domain.W] = (0 until depth).foldLeft(initialState) { (state, level) =>
      // (depth - 1) is the index of the MSB, i.e. 32 bits means bit 31 is MSB
      // level 0 is first bit, level 1 is second bit, etc so `- level` after
      val bitIdxForDirection = (depth - 1) - level

      val fs: FoldState[domain.W] = state.currentSplits.foldLeft(FoldState(Vector.empty, Vector.empty)) { (oldState, block) =>
        val splitIdx = findSplit(block, bitIdxForDirection)
        val (lefts, rights) = block.splitAt(splitIdx)

        (lefts.nonEmpty, rights.nonEmpty) match {
          case (false, false) => throw new IllegalStateException("somehow a node has children but its children don't?")
          case (true, false) =>
            // only going to the left
            FoldState(oldState.nextSplits :+ lefts, oldState.nextSteps :+ Direction.Left)
          case (false, true) =>
            // only going to the right
            FoldState(oldState.nextSplits :+ rights, oldState.nextSteps :+ Direction.Right)
          case (true, true) =>
            // going both directions
            FoldState(oldState.nextSplits :+ lefts :+ rights, oldState.nextSteps :+ Direction.Both)
        }
      }

      val newSteps = state.recipeSteps ++ fs.nextSteps
      LevelState(
        currentSplits = fs.nextSplits,
        recipeSteps = newSteps,
        levelEndpoints = state.levelEndpoints :+ newSteps.length
      )
    }

    // TODO do we even need this?
    val leafIndices = finalState.currentSplits.zipWithIndex.flatMap { (leafBlock, idxOfLeaf) => Vector.fill(leafBlock.length)(idxOfLeaf) }
    
    SequenceRecipe(
      steps = finalState.recipeSteps,
      leafIndices = leafIndices,
      leafCount = finalState.currentSplits.length,
      levelEndpoints = finalState.levelEndpoints
    )
  }

  private def findSplit[W <: Int](block: Vector[UBitInt[W]], bitIdx: Int): Int = {
    @tailrec def go(leftInc: Int, rightExc: Int): Int = {
      // if we've narrowed down to a zero-length range, the split happens where that range starts
      if leftInc >= rightExc then leftInc
      else {
        // we want to do a binary search to see "where the bit we care about flips"
        val mid = leftInc + ((rightExc - leftInc) / 2)
        // if we see our target bit high at mid, we know it flips at or before mid: search [l, mid)
        if (block(mid).toSafeLong & (SafeLong.one << bitIdx)) != SafeLong.zero then go(leftInc, mid)
        // otherwise, search after mid: (mid, r)
        else go(mid + 1, rightExc)
      }
    }
    go(0, block.length)
  }
  
}
