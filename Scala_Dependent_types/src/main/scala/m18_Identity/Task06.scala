package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task06 {
  val A = "A" :: Type
  val A1 = "A1" :: Type
  val A2 = "A2" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A1
  val a2 = "a2" :: A2
  val HId = "HId" :: A1 ~>: A2 ~>: (A1 ->: A2 ->: Type)
  val HIdInd = ("refl" ::: A ~>>: a ~>>: (HId -> HId(A)(A)(a)(a) )) =:: HId
  val hrefl :: HNil = HIdInd.intros
  val a1_eq_a2 = "_ : a1=a2" :: HId(A1)(A2)(a1)(a2)

  val B = "B(_ : A)" :: A ~>: (A ->: Type)
  val f = "f" :: A ~>: a ~>: B(A)(a)

  val indDepMap = HIdInd.induc(A1 :~> (A2 :~> (a1 :~> (a2 :~> (a1_eq_a2 :-> (B ~>: f ~>: HId(B(A1)(a1))(B(A2)(a2))(f(A1)(a1))(f(A2)(a2)) ))))))
  val depMap = indDepMap(A :~> (a :~> (B :~> (f :~> hrefl(B(A)(a))(f(A)(a)) ))))

  def main(args: Array[String]): Unit = {
    depMap !: A1 ~>: A2 ~>: a1 ~>: a2 ~>: (HId(A1)(A2)(a1)(a2) ->: B ~>: f ~>: HId(B(A1)(a1))(B(A2)(a2))(f(A1)(a1))(f(A2)(a2)))
  }
}
