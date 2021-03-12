package m13_Sigma

import provingground.HoTT._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val b = "b" :: B
  val C = "C(_ : A, _ : B)" :: A ->: B ->: Type

  val p = "p" :: Sgma(a !: A, b ~>: C(a)(b))

  val bC = "b->C(a)(b)" :: b ~>: C(a)(b)
  val rec = Sgma(a !: A, b ~>: C(a)(b)).induc(a :~> (bC :-> (b ~>: Sgma(a !: A, C(a)(b)))))

  val f = rec(a :~> (bC :-> (b :~> mkPair(a, bC(b)))))

  def main(args: Array[String]): Unit = {
    f !: Sgma(a !: A, b ~>: C(a)(b)) ->: b ~>: Sgma(a !: A, C(a)(b))
  }
}
