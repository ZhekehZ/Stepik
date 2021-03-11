package m01_BooleanType

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task08 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val C = "C(_ : Boolean)" :: Bool ->: Type
  val c1 = "c1" :: C(tru)
  val c2 = "c2" :: C(fls)

  val A = "A" :: Type
  val a = "a" :: A
  val Id = "_ = _" :: A ~>: (A ->: A ->: Type)
  val IdInd = ("refl" ::: A ~>>: a ~>>: (Id -> Id(A)(a)(a))) =:: Id
  val refl :: HNil = IdInd.intros
  val B = "B" :: Type
  val f = "f" :: A ->: B
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val a3 = "a3" :: A
  //...
  //map !: A ~>: a1 ~>: a2 ~>: (Id(A)(a1)(a2) ->: B ~>: f ~>: Id(B)(f(a1))(f(a2)) )
  //trans !: A ~>: a1 ~>: a2 ~>: (Id(A)(a1)(a2) ->: a3 ~>: (Id(A)(a2)(a3) ->: Id(A)(a1)(a3) ))

  val ind = "ind" :: C ~>: (C(tru) ->: C(fls) ->: b ~>: C(b))
  val beta1 = "ind(c1)(c2)(true)=???" :: C ~>: c1 ~>: c2 ~>: Id(???)(???)(???)
  val beta2 = "ind(c1)(c2)(false)=???" :: C ~>: c1 ~>: c2 ~>: Id(???)(???)(???)

  val constBool = b :-> Bool

  val recBB = ind(constBool)

  val not = recBB(fls)(tru)
  val not_tru_eq_fls = beta1(constBool)(fls)(tru) !: Id(Bool)(not(tru))(fls)
  val not_fls_eq_tru = beta2(constBool)(fls)(tru) !: Id(Bool)(not(fls))(tru)

  val and = b :-> recBB(b)(fls)
  beta1(constBool)(tru)(fls) !: Id(Bool)(and(tru)(tru))(tru)
  beta2(constBool)(tru)(fls) !: Id(Bool)(and(tru)(fls))(fls)
  beta1(constBool)(fls)(fls) !: Id(Bool)(and(fls)(tru))(fls)
  beta2(constBool)(fls)(fls) !: Id(Bool)(and(fls)(fls))(fls)

  //  val not_not_b_eq_b = b :-> Id(Bool)( not(not(b)) )(b)
  //
  //  val lemma1 = map(Bool)( not(tru) )(fls)( not_tru_eq_fls )(Bool)(b :-> not(b))  !: Id(Bool)( not(not(tru)) )( not(fls) )
  //  val lemma2 = trans(Bool)( not(not(tru)) )( not(fls) )(lemma1)(tru)( not_fls_eq_tru ) !: not_not_b_eq_b(tru)
  //
  //  val lemma3 = map(Bool)( not(fls) )(tru)( not_fls_eq_tru )(Bool)(b :-> not(b))  !: Id(Bool)( not(not(fls)) )( not(tru) )
  //  val lemma4 = trans(Bool)( not(not(fls)) )( not(tru) )(lemma3)(fls)( not_tru_eq_fls ) !: not_not_b_eq_b(fls)
  //
  //  val indNot_not_b_eq_b = ind(not_not_b_eq_b)

  def main(args: Array[String]): Unit = {
    //    indNot_not_b_eq_b(lemma2)(lemma4) !: b ~>: not_not_b_eq_b(b)
  }
}
