package m12_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection DuplicatedCode,TypeAnnotation
object Task05 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val B = "B" :: Type
  val b = "b" :: B
  val b1 = "b1" :: B
  val List = "List" :: Type ->: Type
  val ListInd = ("nil" ::: A ~>>: (List -> List(A))) |:
                ("cons" ::: A ~>>: (A ->>: (List :> List(A)) -->>: (List -> List(A)) )) =:: List
  val nil :: cons :: HNil = ListInd.intros
  val as = "as" :: List(A)
  val as1 = "as1" :: List(A)
  val bs = "bs" :: List(B)

  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val one = succ(zero)
  val two = succ(one)

  val Bool = "Boolean" :: Type
  val bool = "bool" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recBoolAAA = BoolInd.rec(A ~>: (A ->: A ->: A))
  val ifElse = recBoolAAA(A :~> (a :-> (a1 :-> a)))(A :~> (a :-> (a1 :-> a1)))

  val indLABool = ListInd.rec(Bool)
  val isNil = indLABool(A :~> tru)(A :~> (a :-> (as :-> (bool :-> fls))))
  val indLAA = ListInd.induc(A :~> (as :-> A))
  val errorEl = "error" :: A
  val head = indLAA(A :~> errorEl)(A :~> (a :-> (as :-> (a1 :-> a))))
  val indLALA = ListInd.induc(A :~> (as :-> List(A) ))
  val errorList = "error" :: List(A)
  val tail = indLALA(A :~> errorList)(A :~> (a :-> (as :-> (as1 :-> as))))

  val indLALBLAB = ListInd.induc(A :~> (as :-> (B ~>: (List(B) ->: List(ProdTyp(A, B))))))

  val zipas = "zip(as)" :: B ~>: (List(B) ->: List(ProdTyp(A, B)))

  val zip = indLALBLAB(A :~> (B :~> (bs :-> nil(ProdTyp(A, B)) )))(
                       A :~> (a :-> (as :-> (zipas :-> (B :~> (bs :->
                         ifElse (isNil(B)(bs)) (List(ProdTyp(A, B)))
                           (nil(ProdTyp(A, B)))
                           (cons (ProdTyp(A, B))
                                 (ProdTyp(A, B).paircons(a)(head (B) (bs)))
                                 (zipas (B) (tail (B) (bs)))
                           )))))))


  def main(args: Array[String]): Unit = {
    val list = cons(Nat)(zero)(cons(Nat)(one)(cons(Nat)(two)(nil(Nat))))
    val list1 = cons(Bool)(tru)(cons(Bool)(fls)(cons(Bool)(tru)(cons(Bool)(fls)(nil(Bool)))))

    assert(zip(Nat)(list)(Bool)(list1) == cons(ProdTyp(Nat, Bool))(
      PairTerm(zero, tru))(
      cons(ProdTyp(Nat, Bool))(
        PairTerm(one, fls))(
        cons(ProdTyp(Nat, Bool))(
          PairTerm(two, tru))(
          nil(ProdTyp(Nat, Bool))
        )
      )
    ))
  }
}
