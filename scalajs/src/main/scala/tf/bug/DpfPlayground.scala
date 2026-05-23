package tf.bug

import calico.IOWebApp
import calico.html.{HtmlAttr, Modifier}
import calico.html.io.{*, given}
import cats.effect
import cats.effect.std.{Random, SecureRandom}
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import dpf.{Correctable, Dpf, Embedding, Sampler, Seeding}
import fs2.dom.HtmlElement
import scodec.bits.BitVector
import tf.bug.dpf.impl.unsafedebug.FreeGroup
import tf.bug.dpf.impl.{Advised, GroupS3, Ranqd1, SBoxPrg}

object DpfPlayground extends IOWebApp {

  val sthree: BitInt[8] = BitInt[8](3)
  val sminusOne: BitInt[8] = BitInt[8](-1)
  val uthree: UBitInt[8] = UBitInt[8](3)
  val uminusOne: UBitInt[8] = UBitInt[8](-1)

  val incrementSbox = SBoxPrg[4](Vector(
    UBitInt[4](0x4),
    UBitInt[4](0x3),
    UBitInt[4](0xf),
    UBitInt[4](0xd),
    UBitInt[4](0xe),
    UBitInt[4](0x1),
    UBitInt[4](0x8),
    UBitInt[4](0x2),
    UBitInt[4](0x7),
    UBitInt[4](0xc),
    UBitInt[4](0x9),
    UBitInt[4](0xa),
    UBitInt[4](0xb),
    UBitInt[4](0x6),
    UBitInt[4](0x5),
    UBitInt[4](0x0),
  ))

  override def render: Resource[effect.IO, HtmlElement[effect.IO]] = {
    type Domain = UBitInt[4]
    type Codomain = GroupS3
    type Seed = Advised[FreeGroup[String]]

    import HtmlShow.given
    Resource.eval(Random.scalaUtilRandomSeedInt[IO](67)).flatMap { rand =>
      val myEmbedding: Embedding[Domain, Seed, Codomain] =
        new FreeGroup.ClearEmbedding(summon, GroupS3.unsafeGroupS3IsDomain, summon, 67)

      Resource.eval(FreeGroup.sampleStringSymbols[IO]).flatMap { stringSymbolSampler =>
        Resource.eval(Dpf.prepare[IO, Seed](stringSymbolSampler)).flatMap { prepared =>
          val prg = new FreeGroup.StringSymbolPrg(2, 0x5EAF00D)
          val myDpf = Dpf.generate[Domain, Seed, Codomain](prepared, UBitInt[4](0xf), GroupS3.CycleUp, myEmbedding, prg)
          println("symbols: " + prg.ctr + " (next symbol is " + FreeGroup.fixBase26(Integer.toString(prg.ctr, 26)) + ")")

          Resource.eval(FreeGroup.sampleStringSymbols[IO]).flatMap { stringSymbolSampler =>
            Resource.eval(Vector.fill(5)(stringSymbolSampler.uniform).sequence).flatMap { symbols =>
              div(
                p(myDpf.toString),
                // p(symbols.combineAll.toString),
                p(myDpf.apply(UBitInt[4](0x0), myEmbedding, prg).toString),
                p(myDpf.apply(UBitInt[4](0x1), myEmbedding, prg).toString),
                p(myDpf.apply(UBitInt[4](0x8), myEmbedding, prg).toString),
                p(myDpf.apply(UBitInt[4](0xa), myEmbedding, prg).toString),
                p(myDpf.apply(UBitInt[4](0xf), myEmbedding, prg).toString),
              )
            }
          }
        }
      }
    }
  }

//  def old1 = {
//    val seeding = Seeding[SBoxPrg[4]]
//    val correction = Correctable[seeding.Seed]
//    val seed0f = seeding.randomSeed[IO](incrementSbox, rand)
//    val seed1f = seeding.randomSeed[IO](incrementSbox, rand)
//
//    val sharedCycle = Sharing[GroupS3]
//    val cyclef = sharedCycle.share(GroupS3.CycleUp, 3, rand)
//
//    Resource.eval((seed0f, seed1f, cyclef).parTupled).flatMap { (seed0, seed1, cycleshares) =>
//      val (seed00, seed01) = seeding.expand(incrementSbox, seed0)
//      val (seed000, seed001) = seeding.expand(incrementSbox, seed00)
//      val (seed010, seed011) = seeding.expand(incrementSbox, seed01)
//
//      mainTag(
//        h1("DPF Playground"),
//        p("seed0: ", seed0, " seed1: ", seed1),
//        h1(seed0),
//        p(seed00, ", ", seed01),
//        p(seed000, ", ", seed001, ", ", seed010, ", ", seed011),
//        p(cycleshares(0), cycleshares(1), cycleshares(2)),
//      )
//    }
//  }

}
