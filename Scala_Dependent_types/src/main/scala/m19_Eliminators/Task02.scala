package m19_Eliminators

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task02 {
  val A = "A" :: Type
  val a = "a" :: A

  val Bool = "Boolean" :: Type
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val BTreeA = "BTree(A)" :: Type
  val BTreeAInd = ("leaf" ::: BTreeA) |: ("fork" ::: BTreeA -->>: A ->>: BTreeA -->>: BTreeA) =: BTreeA
  val leaf :: fork :: HNil = BTreeAInd.intros

  val D = "D(_ : BTree(A))" :: BTreeA ->: Type
  val l = "left" :: BTreeA
  val r = "right" :: BTreeA
  val t = "tree" :: BTreeA

  val T = D(leaf) ->: ((l ~>: (D(l) ->: (a ~>: (r ~>: (D(r) ->: D((fork)(l)(a)(r)) ))))) ->: (t ~>: D(t)))

  def main(args: Array[String]): Unit = {
    BTreeAInd.induc(D) !: T
  }
}
