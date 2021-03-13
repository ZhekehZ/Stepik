package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task02 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val a3 = "a3" :: A
  val Id = "Id" :: A ->: A ->: Type
  val IdInd = ("refl" ::: a ~>>: (Id -> Id(a)(a) )) =:: Id
  val refl :: HNil = IdInd.intros

  val a1_eq_a2 = "_ : a1=a2" :: Id(a1)(a2)

  val ind_a1eqa2_a2eqa3_a1eqa3 = IdInd.induc(a1 :~> (a2 :~> (a1_eq_a2 :-> (a3 ~>: (Id(a2)(a3) ->: Id(a1)(a3) )))))
  val a_eq_a3 = "_ : a=a3" :: Id(a)(a3)
  val trans = ind_a1eqa2_a2eqa3_a1eqa3(a :~> (a3 :~> (a_eq_a3 :-> a_eq_a3 )))

  def main(args: Array[String]): Unit = {
    trans !: a1 ~>: (a2 ~>: (Id(a1)(a2) ->: (a3 ~>: (Id(a2)(a3) ->: Id(a1)(a3) ))))
  }
}
