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

  val aesSbox = SBoxPrg[8](Vector(
    UBitInt[8](0x63),
    UBitInt[8](0x7c),
    UBitInt[8](0x77),
    UBitInt[8](0x7b),
    UBitInt[8](0xf2),
    UBitInt[8](0x6b),
    UBitInt[8](0x6f),
    UBitInt[8](0xc5),
    UBitInt[8](0x30),
    UBitInt[8](0x01),
    UBitInt[8](0x67),
    UBitInt[8](0x2b),
    UBitInt[8](0xfe),
    UBitInt[8](0xd7),
    UBitInt[8](0xab),
    UBitInt[8](0x76),
    UBitInt[8](0xca),
    UBitInt[8](0x82),
    UBitInt[8](0xc9),
    UBitInt[8](0x7d),
    UBitInt[8](0xfa),
    UBitInt[8](0x59),
    UBitInt[8](0x47),
    UBitInt[8](0xf0),
    UBitInt[8](0xad),
    UBitInt[8](0xd4),
    UBitInt[8](0xa2),
    UBitInt[8](0xaf),
    UBitInt[8](0x9c),
    UBitInt[8](0xa4),
    UBitInt[8](0x72),
    UBitInt[8](0xc0),
    UBitInt[8](0xb7),
    UBitInt[8](0xfd),
    UBitInt[8](0x93),
    UBitInt[8](0x26),
    UBitInt[8](0x36),
    UBitInt[8](0x3f),
    UBitInt[8](0xf7),
    UBitInt[8](0xcc),
    UBitInt[8](0x34),
    UBitInt[8](0xa5),
    UBitInt[8](0xe5),
    UBitInt[8](0xf1),
    UBitInt[8](0x71),
    UBitInt[8](0xd8),
    UBitInt[8](0x31),
    UBitInt[8](0x15),
    UBitInt[8](0x04),
    UBitInt[8](0xc7),
    UBitInt[8](0x23),
    UBitInt[8](0xc3),
    UBitInt[8](0x18),
    UBitInt[8](0x96),
    UBitInt[8](0x05),
    UBitInt[8](0x9a),
    UBitInt[8](0x07),
    UBitInt[8](0x12),
    UBitInt[8](0x80),
    UBitInt[8](0xe2),
    UBitInt[8](0xeb),
    UBitInt[8](0x27),
    UBitInt[8](0xb2),
    UBitInt[8](0x75),
    UBitInt[8](0x09),
    UBitInt[8](0x83),
    UBitInt[8](0x2c),
    UBitInt[8](0x1a),
    UBitInt[8](0x1b),
    UBitInt[8](0x6e),
    UBitInt[8](0x5a),
    UBitInt[8](0xa0),
    UBitInt[8](0x52),
    UBitInt[8](0x3b),
    UBitInt[8](0xd6),
    UBitInt[8](0xb3),
    UBitInt[8](0x29),
    UBitInt[8](0xe3),
    UBitInt[8](0x2f),
    UBitInt[8](0x84),
    UBitInt[8](0x53),
    UBitInt[8](0xd1),
    UBitInt[8](0x00),
    UBitInt[8](0xed),
    UBitInt[8](0x20),
    UBitInt[8](0xfc),
    UBitInt[8](0xb1),
    UBitInt[8](0x5b),
    UBitInt[8](0x6a),
    UBitInt[8](0xcb),
    UBitInt[8](0xbe),
    UBitInt[8](0x39),
    UBitInt[8](0x4a),
    UBitInt[8](0x4c),
    UBitInt[8](0x58),
    UBitInt[8](0xcf),
    UBitInt[8](0xd0),
    UBitInt[8](0xef),
    UBitInt[8](0xaa),
    UBitInt[8](0xfb),
    UBitInt[8](0x43),
    UBitInt[8](0x4d),
    UBitInt[8](0x33),
    UBitInt[8](0x85),
    UBitInt[8](0x45),
    UBitInt[8](0xf9),
    UBitInt[8](0x02),
    UBitInt[8](0x7f),
    UBitInt[8](0x50),
    UBitInt[8](0x3c),
    UBitInt[8](0x9f),
    UBitInt[8](0xa8),
    UBitInt[8](0x51),
    UBitInt[8](0xa3),
    UBitInt[8](0x40),
    UBitInt[8](0x8f),
    UBitInt[8](0x92),
    UBitInt[8](0x9d),
    UBitInt[8](0x38),
    UBitInt[8](0xf5),
    UBitInt[8](0xbc),
    UBitInt[8](0xb6),
    UBitInt[8](0xda),
    UBitInt[8](0x21),
    UBitInt[8](0x10),
    UBitInt[8](0xff),
    UBitInt[8](0xf3),
    UBitInt[8](0xd2),
    UBitInt[8](0xcd),
    UBitInt[8](0x0c),
    UBitInt[8](0x13),
    UBitInt[8](0xec),
    UBitInt[8](0x5f),
    UBitInt[8](0x97),
    UBitInt[8](0x44),
    UBitInt[8](0x17),
    UBitInt[8](0xc4),
    UBitInt[8](0xa7),
    UBitInt[8](0x7e),
    UBitInt[8](0x3d),
    UBitInt[8](0x64),
    UBitInt[8](0x5d),
    UBitInt[8](0x19),
    UBitInt[8](0x73),
    UBitInt[8](0x60),
    UBitInt[8](0x81),
    UBitInt[8](0x4f),
    UBitInt[8](0xdc),
    UBitInt[8](0x22),
    UBitInt[8](0x2a),
    UBitInt[8](0x90),
    UBitInt[8](0x88),
    UBitInt[8](0x46),
    UBitInt[8](0xee),
    UBitInt[8](0xb8),
    UBitInt[8](0x14),
    UBitInt[8](0xde),
    UBitInt[8](0x5e),
    UBitInt[8](0x0b),
    UBitInt[8](0xdb),
    UBitInt[8](0xe0),
    UBitInt[8](0x32),
    UBitInt[8](0x3a),
    UBitInt[8](0x0a),
    UBitInt[8](0x49),
    UBitInt[8](0x06),
    UBitInt[8](0x24),
    UBitInt[8](0x5c),
    UBitInt[8](0xc2),
    UBitInt[8](0xd3),
    UBitInt[8](0xac),
    UBitInt[8](0x62),
    UBitInt[8](0x91),
    UBitInt[8](0x95),
    UBitInt[8](0xe4),
    UBitInt[8](0x79),
    UBitInt[8](0xe7),
    UBitInt[8](0xc8),
    UBitInt[8](0x37),
    UBitInt[8](0x6d),
    UBitInt[8](0x8d),
    UBitInt[8](0xd5),
    UBitInt[8](0x4e),
    UBitInt[8](0xa9),
    UBitInt[8](0x6c),
    UBitInt[8](0x56),
    UBitInt[8](0xf4),
    UBitInt[8](0xea),
    UBitInt[8](0x65),
    UBitInt[8](0x7a),
    UBitInt[8](0xae),
    UBitInt[8](0x08),
    UBitInt[8](0xba),
    UBitInt[8](0x78),
    UBitInt[8](0x25),
    UBitInt[8](0x2e),
    UBitInt[8](0x1c),
    UBitInt[8](0xa6),
    UBitInt[8](0xb4),
    UBitInt[8](0xc6),
    UBitInt[8](0xe8),
    UBitInt[8](0xdd),
    UBitInt[8](0x74),
    UBitInt[8](0x1f),
    UBitInt[8](0x4b),
    UBitInt[8](0xbd),
    UBitInt[8](0x8b),
    UBitInt[8](0x8a),
    UBitInt[8](0x70),
    UBitInt[8](0x3e),
    UBitInt[8](0xb5),
    UBitInt[8](0x66),
    UBitInt[8](0x48),
    UBitInt[8](0x03),
    UBitInt[8](0xf6),
    UBitInt[8](0x0e),
    UBitInt[8](0x61),
    UBitInt[8](0x35),
    UBitInt[8](0x57),
    UBitInt[8](0xb9),
    UBitInt[8](0x86),
    UBitInt[8](0xc1),
    UBitInt[8](0x1d),
    UBitInt[8](0x9e),
    UBitInt[8](0xe1),
    UBitInt[8](0xf8),
    UBitInt[8](0x98),
    UBitInt[8](0x11),
    UBitInt[8](0x69),
    UBitInt[8](0xd9),
    UBitInt[8](0x8e),
    UBitInt[8](0x94),
    UBitInt[8](0x9b),
    UBitInt[8](0x1e),
    UBitInt[8](0x87),
    UBitInt[8](0xe9),
    UBitInt[8](0xce),
    UBitInt[8](0x55),
    UBitInt[8](0x28),
    UBitInt[8](0xdf),
    UBitInt[8](0x8c),
    UBitInt[8](0xa1),
    UBitInt[8](0x89),
    UBitInt[8](0x0d),
    UBitInt[8](0xbf),
    UBitInt[8](0xe6),
    UBitInt[8](0x42),
    UBitInt[8](0x68),
    UBitInt[8](0x41),
    UBitInt[8](0x99),
    UBitInt[8](0x2d),
    UBitInt[8](0x0f),
    UBitInt[8](0xb0),
    UBitInt[8](0x54),
    UBitInt[8](0xbb),
    UBitInt[8](0x16),
  ))

  override def render: Resource[effect.IO, HtmlElement[effect.IO]] = {
    type Domain = UBitInt[4]
    type Codomain = UBitInt[2]
    type Seed = BitVecN[8]

    import HtmlShow.given
    Resource.eval(Random.scalaUtilRandomSeedInt[IO](67)).flatMap { rand =>
      val myEmbedding: Embedding[Domain, Seed, Codomain] =
        Embedding.uadditiveSharePacking

      type L = myEmbedding.L

      Resource.eval(SecureRandom.javaSecuritySecureRandom[IO]).flatMap { secureRandom =>
        Resource.eval(Dpf.prepare[IO, Seed](secureRandom)).flatMap { prepared =>
          val prg = aesSbox
          val myDpf = Dpf.generate[Domain, Seed, L, Codomain](prepared, UBitInt[4](0x8), UBitInt[2](0x1), myEmbedding, prg)
          // println("symbols: " + prg.ctr + " (next symbol is " + FreeGroup.fixBase26(Integer.toString(prg.ctr, 26)) + ")")

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
