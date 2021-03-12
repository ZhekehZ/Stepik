package m12_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task02 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val B = "B" :: Type
  val List = "List" :: Type ->: Type
  val ListInd = ("nil" ::: A ~>>: (List -> List(A))) |:
                ("cons" ::: A ~>>: (A ->>: (List :> List(A)) -->>: (List -> List(A)) )) =:: List
  val nil :: cons :: HNil = ListInd.intros
  val as = "as" :: List(A)
  val Nat = "Nat" :: Type
  val n = "n" :: Nat

  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)

  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recBB = BoolInd.rec(Bool)
  val not = recBB(fls)(tru)
  val recBoolAAA = BoolInd.rec(A ~>: (A ->: A ->: A))
  val ifElse = recBoolAAA(A :~> (a :-> (a1 :-> a)))(A :~> (a :-> (a1 :-> a1)))
  val recNB = NatInd.rec(Bool)
  val isEven = recNB(tru)(n :-> (b :-> not(b)))
  val indLApLA = ListInd.induc(A :~> (as :-> ((A ->: Bool) ->: List(A) )))
  val p = "p" :: A ->: Bool

  val filteras = "filter(as)" :: (A ->: Bool) ->: List(A)

  val filter = indLApLA(A :~> (p :-> nil(A) ))(
                        A :~> (a :-> (as :-> (filteras :-> (p :->
                            ifElse(p(a)) (List(A)) (cons(A)(a)(filteras(p)))(filteras(p)) )))))

  def main(args: Array[String]): Unit = {
    val list = cons(Nat)(zero)(cons(Nat)(one)(cons(Nat)(two)(nil(Nat))))
    assert(filter(Nat)(list)(isEven) == cons(Nat)(zero)(cons(Nat)(two)(nil(Nat))))
  }
}
