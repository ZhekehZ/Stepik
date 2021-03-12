package m10_Function

import provingground.HoTT._

//noinspection TypeAnnotation
object Task02 {
  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val b = "b" :: B
  val C = "C" :: Type
  val f = "f" :: A ->: B ->: C

  val swap = f :-> (b :-> (a :-> f(a)(b)))

  def main(args: Array[String]): Unit = {
    swap !:(A ->: B ->: C) ->: B ->: A ->: C
  }
}
