package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task08 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat
  val m = "m" :: Nat

  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)
  val seven = succ(six)
  val eight = succ(seven)
  val nine = succ(eight)

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn = "add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))
  val multn = "mult(n)" :: Nat ->: Nat
  val mult = recNNN(m :-> zero)(n :-> (multn :-> (m :-> add(multn(m))(m) )))

  val powm = "pow(_, m)" :: Nat ->: Nat
  val pow_flip = recNNN(n :-> one)(m :-> (powm :-> (n :-> mult(powm(n))(n) )))
  val pow = n :-> (m :-> pow_flip(m)(n) )

  def main(args: Array[String]): Unit = {
    assert(pow(two)(three) == eight)
    assert(pow(zero)(three) == zero)
    assert(pow(one)(three) == one)
    assert(pow(three)(two) == nine)
  }
}
