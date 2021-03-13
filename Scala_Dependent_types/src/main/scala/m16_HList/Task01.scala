package m16_HList

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val a = "a" :: A
  val A1 = "A1" :: Type
  val a1 = "a1" :: A1
  val A2 = "A2" :: Type
  val a2 = "a2" :: A2

  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)

  val TLst = "TList" :: Type
  val TLstInd = ("nil" ::: TLst) |: ("cons" ::: Type ->>: TLst -->>: TLst) =: TLst
  val tnil :: tcons :: HNil = TLstInd.intros
  val tlst = "tlist" :: TLst
  val HLst = "HList" :: TLst ->: Type
  val HLstInd = ("nil" ::: HLst -> HLst(tnil) ) |:
                ("cons" ::: A ~>>: (A ->>: (tlst ~>>: ((HLst :> HLst(tlst) ) -->>: (HLst -> HLst(tcons(A)(tlst)) ))))) =:: HLst
  val hnil :: hcons :: HNil = HLstInd.intros

  val t = "t" :: HLst(tlst)
  val recHN = HLstInd.rec(Nat)

  val hsize = recHN(zero)(
              A :~> (a :-> (tlst :~> (t :-> (n :-> succ(n) ))))) !: tlst ~>: (HLst(tlst) ->: Nat)

  def main(args: Array[String]): Unit = {
    val hlist = hcons(A)(a)(tcons(A1)(tcons(A2)(tnil)))(hcons(A1)(a1)(tcons(A2)(tnil))(hcons(A2)(a2)(tnil)(hnil))) !: HLst(tcons(A)(tcons(A1)(tcons(A2)(tnil))))
    assert(hsize(tcons(A)(tcons(A1)(tcons(A2)(tnil))))(hlist) == three)
  }
}
