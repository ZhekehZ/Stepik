package m11_Empty_Unit

import provingground.HoTT._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val a = "a" :: A
  val f = "f" :: PlusTyp(A, A ->: Zero) ->: Zero

  val pANA = PlusTyp(A, A ->: Zero)
  val f1 = a :-> f(pANA.incl1(a))
  val f2 = f :-> f(pANA.incl2(a :-> f(pANA.incl1(a))))

  def main(args: Array[String]): Unit = {
    f1 !: A ->: Zero
    f2 !: (PlusTyp(A, A ->: Zero) ->: Zero) ->: Zero
  }
}
