package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task09 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val b1 = "b1" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val recBB = BoolInd.rec(Bool)
  val not = recBB(fls)(tru)
  val recBBB = BoolInd.rec(Bool ->: Bool)
  val or = recBBB(b :-> tru)(b :-> b)
  val and = recBBB(b :-> b)(b :-> fls)

  val indB_deMorgan = BoolInd.induc(b :-> (b1 ~>: (not(and(b)(b1)) =:= or(not(b))(not(b1)) )))
  val deMorgan = indB_deMorgan(b1 :~> not(b1).refl)(b1 :~> tru.refl)

  def main(args: Array[String]): Unit = {
    deMorgan !: b ~>: (b1 ~>: (not(and(b)(b1)) =:= or(not(b))(not(b1)) ))
  }
}
