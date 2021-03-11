package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task12 {
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
  val nine = succ(eight)

  val n = "n" :: Nat
  val m = "m" :: Nat

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn = "add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val recNNNN = NatInd.rec(Nat ->: Nat ->: Nat)
  val m1 = "fib(n)" :: Nat
  val m2 = "fib(n+1)" :: Nat
  val fibn = "fib_aux(n,_,_)" :: Nat ->: Nat ->: Nat

  val fib_aux = recNNNN(m1 :-> (m2 :-> m1))(n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(add(m1)(m2)) ))))

  val fib = n :-> fib_aux(n)(zero)(one)

  def main(args: Array[String]): Unit = {
    assert(fib(zero) == zero)
    assert(fib(one) == one)
    assert(fib(two) == one)
    assert(fib(three) == two)
  }
}
