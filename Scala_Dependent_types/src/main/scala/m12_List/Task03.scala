package m12_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task03 {
  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val List = "List" :: Type ->: Type
  val ListInd = ("nil" ::: A ~>>: (List -> List(A))) |:
                ("cons" ::: A ~>>: (A ->>: (List :> List(A)) -->>: (List -> List(A)) )) =:: List
  val nil :: cons :: HNil = ListInd.intros
  val as = "as" :: List(A)
  val Nat = "Nat" :: Type
  val n = "n" :: Nat
  val m = "m" :: Nat
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn ="add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val op = "op" :: B ->: A ->: B
  val seed = "seed" :: B
  val foldlas = "foldl(as)" :: B ~>: ((B ->: A ->: B) ->: B ->: B)

  val indLAopBB = ListInd.induc(A :~> (as :-> (B ~>: ((B ->: A ->: B) ->: B ->: B))))

  val foldl = indLAopBB(A :~> (B :~> (op :-> (seed :-> seed))))(
                        A :~> (a :-> (as :-> (foldlas :-> (B :~> (op :-> (seed :->
                          foldlas(B)(op)(op(seed)(a)) )))))))

  def main(args: Array[String]): Unit = {
    val list = cons(Nat)(zero)(cons(Nat)(one)(cons(Nat)(two)(nil(Nat))))
    assert(foldl(Nat)(list)(Nat)(add)(zero) == three)
  }
}
