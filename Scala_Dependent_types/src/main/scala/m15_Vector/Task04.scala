package m15_Vector

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task04 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros

  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)
  val seven = succ(six)
  val eight = succ(seven)

  val n = "n" :: Nat
  val m = "m" :: Nat

  val recNN = NatInd.rec(Nat)
  val errorN = "error" :: Nat
  val pred = recNN(zero)(n :-> (m :-> n))

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn ="add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val multn ="mult(n)" :: Nat ->: Nat
  val mult = recNNN(m :-> zero)(n :-> (multn :-> (m :-> add(multn(m))(m) )))

  val Vec = "Vec" :: Nat ->: Type
  val VecInd = ("nil" ::: (Vec -> Vec(zero))) |:
               ("cons" ::: n ~>>: (Nat ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))) =:: Vec
  val vnil :: vcons :: HNil = VecInd.intros

  val vn = "v_n" :: Vec(n)
  val wn = "w_n" :: Vec(n)
  val vm = "v_m" :: Vec(m)

  val a = "a" :: Nat
  val a1 = "a1" :: Nat
  val a2 = "a2" :: Nat
  val a3 = "a3" :: Nat
  val a4 = "a4" :: Nat

  val recVN = VecInd.rec(Nat)
  val errorEl = "error" :: Nat
  val vhead = recVN(errorEl)(n :~> (a :-> (vn :-> (a1 :-> a))))

  val indVnVpn = VecInd.induc(n :~> (vn :-> Vec(pred(n)) ))
  val errorList = "error" :: Vec(pred(zero))
  val vpn = "v_(pred(n))" :: Vec(pred(n))
  val vtail = indVnVpn(errorList)(n :~> (a :-> (vn :-> (vpn :-> vn))))

  val v0 = "v_0" :: Vec(zero)
  val w0 = "w_0" :: Vec(zero)
  val vsn = "v_(succ(n))" :: Vec(succ(n))
  val wsn = "w_(succ(n))" :: Vec(succ(n))

  val indNVnVnN = NatInd.induc(n :-> (Vec(n) ->: (Vec(n) ->: Nat)))

  val vscalarProdn = "vscalarProd(n)" :: (Vec(n) ->: (Vec(n) ->: Nat))

  val vscalarProd = indNVnVnN(
            v0 :-> (w0 :-> zero)) (
            n :~> (vscalarProdn :-> (vsn :-> (wsn :->
              add
                (mult (vhead (succ(n)) (vsn)) (vhead (succ(n)) (wsn)))
                (vscalarProdn (vtail (succ(n)) (vsn)) (vtail (succ(n)) (wsn)))
              ))))

  def main(args: Array[String]): Unit = {
    val vect1 = vcons(one)(one)(vcons(zero)(two)(vnil))
    val vect2 = vcons(one)(two)(vcons(zero)(three)(vnil))
    assert(vscalarProd(two)(vect1)(vect2) == eight)
    assert(vscalarProd(zero)(vnil)(vnil) == zero)
    assert(vscalarProd(one)(vcons(zero)(one)(vnil))(vcons(zero)(three)(vnil)) == three)
  }
}
