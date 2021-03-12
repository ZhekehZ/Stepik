package m12_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection DuplicatedCode,TypeAnnotation
object Task01 {

  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val List = "List" :: Type ->: Type
  val ListInd = ("nil" ::: A ~>>: (List -> List(A))) |:
                ("cons" ::: A ~>>: (A ->>: (List :> List(A)) -->>: (List -> List(A)) )) =:: List
  val nil :: cons :: HNil = ListInd.intros
  val as = "as" :: List(A)
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val indLAfLB = ListInd.induc(A :~> (as :-> (B ~>: ((A ->: B) ->: List(B) ))))
  val f = "f" :: A ->: B

  val mapas = "map(as)" :: B ~>: ((A ->: B) ->: List(B))

  val map = indLAfLB(A :~> (B :~> (f :-> nil(B))))(
                     A :~> (a :-> (as :-> (mapas :-> (B :~> (f :-> cons(B)(f(a))(mapas(B)(f)) ))))))

  def main(args: Array[String]): Unit = {
    val list = cons(Nat)(zero)(cons(Nat)(one)(cons(Nat)(two)(nil(Nat))))
    assert(map(Nat)(list)(Nat)(succ) == cons(Nat)(one)(cons(Nat)(two)(cons(Nat)(three)(nil(Nat)))))
  }

}
