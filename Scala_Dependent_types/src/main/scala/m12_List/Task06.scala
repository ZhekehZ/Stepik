package m12_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task06 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val List = "List" :: Type ->: Type
  val ListInd = ("nil" ::: A ~>>: (List -> List(A))) |:
                ("cons" ::: A ~>>: (A ->>: (List :> List(A)) -->>: (List -> List(A)) )) =:: List
  val nil :: cons :: HNil = ListInd.intros
  val as = "as" :: List(A)
  val as1 = "as1" :: List(A)

  val Bool = "Boolean" :: Type
  val bool = "bool" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recBBB = BoolInd.rec(Bool ->: Bool)
  val and = recBBB(bool :-> bool)(bool :-> fls)
  val recBoolAAA = BoolInd.rec(A ~>: (A ->: A ->: A))
  val ifElse = recBoolAAA(A :~> (a :-> (a1 :-> a)))(A :~> (a :-> (a1 :-> a1)))

  val Nat = "Nat" :: Type
  val n = "n" :: Nat
  val m = "m" :: Nat
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)

  val recNB = NatInd.rec(Bool)
  val isZero = recNB(tru)(n :-> (bool :-> fls))
  val recNN = NatInd.rec(Nat)
  val pred = recNN(zero)(n :-> (m :-> n))
  val recNNB = NatInd.rec(Nat ->: Bool)
  val isEqualn = "isEqual(n)" :: Nat ->: Bool
  val isEqualNat = recNNB(m :->
    isZero(m)
  )(n :-> (isEqualn :-> (m :->
    ifElse(isZero(m))(Bool)(fls)(isEqualn(pred(m))) ))
  )

  val recLABool = ListInd.rec(Bool)
  val isNil = recLABool(A :~> tru)(A :~> (a :-> (as :-> (bool :-> fls))))
  val indLAA = ListInd.induc(A :~> (as :-> A))
  val errorEl = "error" :: A
  val head = indLAA(A :~> errorEl)(A :~> (a :-> (as :-> (a1 :-> a))))
  val indLALA = ListInd.induc(A :~> (as :-> List(A) ))
  val errorList = "error" :: List(A)
  val tail = indLALA(A :~> errorList)(A :~> (a :-> (as :-> (as1 :-> as))))

  val indLALABool = ListInd.induc(A :~> (as :-> (List(A) ->: (A ->: A ->: Bool) ->: Bool)))
  val isEqualEl = "isEqual_A" :: A ->: A ->: Bool
  val isEqualas = "isEqual(as)" :: List(A) ->: (A ->: A ->: Bool) ->: Bool

  val isEqual = indLALABool(A :~> (as1 :-> (isEqualEl :-> isNil (A) (as1) )))(
              A :~> (a :-> (as :-> (isEqualas :-> (as1 :-> (isEqualEl :->
                ifElse (isNil (A) (as1)) (Bool)
                  (fls)
                  (ifElse (isEqualEl(a)(head (A) (as1))) (Bool)
                    (isEqualas (tail (A) (as1)) (isEqualEl))
                    (fls)
                  )
                ))))))

  def main(args: Array[String]): Unit = {
    val list = cons(Nat)(one)(cons(Nat)(two)(nil(Nat)))
    assert(isEqual(Nat)(list)(list)(isEqualNat) == tru)
  }
}
