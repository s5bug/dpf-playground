package tf.bug.dpf

import cats.{Applicative, Monad}

abstract class ObliviousSelection[+F[_], A] {
  type PartyContext
  def prepareOneRound: F[(PartyContext, PartyContext)]

  type Payload
  final type Result = ObliviousSelection.Result[Payload, A]

  def makeSelection(me: Party, myContext: PartyContext, myBitShare: Boolean, myFalseShare: A, myTrueShare: A): F[Result]
}

object ObliviousSelection {

  abstract class Result[P, A] {
    val send: P
    def receive(received: P): A
  }

  type Aux[+F[_], A, P] = ObliviousSelection[F, A] { type Payload = P }

  def ofRing[F[_], A](sampler: Sampler[F, A])(using r: spire.algebra.Ring[A], sampleThenShare: Monad[F]): Aux[F, A, (A, A)] = new ObliviousSelection[F, A] {
    override type PartyContext = BeaverTriple[A]

    override def prepareOneRound: F[(PartyContext, PartyContext)] =
      sampleThenShare.flatMap(BeaverTriple.fromCoefficients(sampler).uniform)(_.share(sampler)(using r.additive, sampleThenShare))

    override type Payload = (A, A)

    // we can do du-atallah-based OS over any ring
    // note that our target value = (bit * true) + ((1 - bit) * false)
    // note that bit = pa xor pb = pa + pb - 2papb
    // true = ta + tb, false = fa + fb
    // so we want to end up with Z = f + bit*(t - f)
    // = (fa + fb) + (pa + pb - 2papb)(ta + tb - fa - fb)
    // = (fa + fb) + (pa + pb - 2papb)((ta - fa) + (tb - fb))
    // = (fa + fb) + pa(da) + pa(db) + pb(da) + pb(db) - 2papb(da) - 2papb(db)
    // we can split this into three parts
    // = [fa + pa(da)] + [fb + pb(db)] + [pa(db) + pb(da) - 2papb(da) - 2papb(db)]
    // we can do multiplications locally to save on rounds: Xa = pa(da) and Xb = pb(db)
    // = [fa + Xa] + [fb + Xb] + [pa(db) + pb(da) - 2pbXa - 2paXb]
    // = [fa + Xa] + [fb + Xb] + pa[db - 2Xb] + pb[da - 2Xa]
    // now we only have two cross-party multiplications, but we can actually reduce this to one:
    // = [fa + Xa - pa[da - 2Xa]] + [fb + Xb - pb[db - 2Xb]] + (pa + pb)([da - 2Xa] + [db - 2Xb])
    // define Y = d - 2X
    // = [fa + Xa - paYa] + [fb + Xb - pbYb] + (pa + pb)(Ya + Yb)
    // if we have a precalculated beaver triple (ca + cb) = (aa + ab) * (ba + bb), then party A will get sent (pb - ab) and (Yb - bb) and can calculate
    // (pa - aa + pb - ab) * ba = paba - aaba + pbba - abba
    // aa * (Ya - ba + Yb - bb) = aaYa - aaba + aaYb - aabb
    // party B will get sent (pa - aa) and (Ya - ba) can calculate:
    // (pa - aa + pb - ab) * bb = pabb - aabb + pbbb - abbb
    // ab * (Ya - ba + Yb - bb) = abYa - abba + abYb - abbb
    // they can each calculate a crossShare
    // crossShareA = ca + (p - a)ba + aa(Y - b)
    // crossShareB = cb + (p - b)bb + ab(Y - b)
    // these shares aren't complete yet: crossShareA + crossShareB
    // these shares aren't complete yet: crossShareA + crossShareB
    // = (ca + cb) + (p - a)(ba + bb) + (aa + ab)(Y - b)
    // = c + (p - a)b + a(Y - b)
    // = ab + bp - ab + aY - ab
    // = bp + aY - ab
    // note that our target is pY, so ideally one party adds a term (ab - bp - aY + pY) = (p - a)(Y - b)
    // because party A was sent (pb - ab) and (Yb - bb), and has (pa - aa) and (Ya - ba), it can just add and multiply
    // crossShareA' = crossShareA + ((pb - ab) + (pa - aa))((Yb - bb) + (Ya - ba))
    // crossShareA' + crossShareB = pY
    override def makeSelection(me: Party, beaverShare: PartyContext, myBitShare: Boolean, myFalseShare: A, myTrueShare: A): F[this.Result] = {
      // writing from the perspective of party A,
      // myFalseShare is fa
      // pa
      val boolToArith = if myBitShare then r.one else r.zero
      // da
      val difference = r.minus(myTrueShare, myFalseShare)
      // Xa
      val x = r.times(boolToArith, difference)
      // Ya
      val y = r.minus(difference, r.sumN(x, 2))

      // pa - aa
      val pMasked = r.minus(boolToArith, beaverShare.a)
      // Ya - ba
      val yMasked = r.minus(y, beaverShare.b)

      sampleThenShare.pure(new this.Result {
        override val send: (A, A) = (pMasked, yMasked)

        override def receive(received: (A, A)): A = {
          val (pOtherMasked, yOtherMasked) = received

          // (p - a)
          val pPlaintextMinusA = r.plus(pMasked, pOtherMasked)
          // (Y - b)
          val yPlaintextMinusB = r.plus(yMasked, yOtherMasked)

          // (p - a)ba
          val crossSharePPart = r.times(pPlaintextMinusA, beaverShare.b)
          // aa(Y - b)
          val crossShareYPart = r.times(beaverShare.a, yPlaintextMinusB)
          // ca + (p - a)ba + aa(Y - b)
          val preCrossShare = r.plus(beaverShare.c, r.plus(crossSharePPart, crossShareYPart))

          val crossShare = if me == Party.Party0 then {
            // (p - a)(Y - b)
            val correction = r.times(pPlaintextMinusA, yPlaintextMinusB)
            r.plus(preCrossShare, correction)
          } else preCrossShare

          // paYa
          val py = r.times(boolToArith, y)
          // (f + Xa) - paYa
          val localOffset = r.minus(r.plus(myFalseShare, x), py)

          r.plus(localOffset, crossShare)
        }
      })
    }
  }

}
