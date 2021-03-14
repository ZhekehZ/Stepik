package m19_Eliminators

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val C = "C" :: Type

  val Bool = "Boolean" :: Type
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val BTreeA = "BTree(A)" :: Type
  val BTreeAInd = ("leaf" ::: BTreeA) |: ("fork" ::: BTreeA -->>: A ->>: BTreeA -->>: BTreeA) =: BTreeA
  val leaf :: fork :: HNil = BTreeAInd.intros

  val T = C ->: (BTreeA ->: C ->: A ->: BTreeA ->: C ->: C) ->:  BTreeA ->: C

  def main(args: Array[String]): Unit = {
    BTreeAInd.rec(C) !: T
  }
}
