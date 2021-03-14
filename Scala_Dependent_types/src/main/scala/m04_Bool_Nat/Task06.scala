package m04_Bool_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task06 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat
  val m = "m" :: Nat

  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)
  val seven = succ(six)
  val eight = succ(seven)
  val nine = succ(eight)

  val C = "C" :: Type
  val c  = "c" :: C
  val f = "f" :: Nat ->: C ->: C

  val A = "A" :: Type
  val a = "a" :: A
  val Id = "Id" :: A ~>: (A ->: A ->: Type)
  val IdInd = ("refl" ::: A ~>>: a ~>>: (Id -> Id(A)(a)(a) )) =:: Id
  val refl :: HNil = IdInd.intros

  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val a3 = "a3" :: A
  val B = "B" :: Type
  val g = "g" :: A ->: B

  val h = "h" :: A ->: B

  val a1_eq_a2 = "a1 = a2" :: Id(A)(a1)(a2)
  val ind_a1eqa2_fa1eqfa2 = IdInd.induc(A :~> (a1 :~> (a2 :~> (a1_eq_a2 :-> (B ~>: h ~>: Id(B)(h(a1))(h(a2)) )))))
  val map = ind_a1eqa2_fa1eqfa2(A :~> (a :~> (B :~> (h :~> refl(B)(h(a)) ))))

  val ind_a1eqa2_a2eqa3_a1eqa3 = IdInd.induc(A :~> (a1 :~> (a2 :~> (a1_eq_a2 :-> (a3 ~>: (Id(A)(a2)(a3) ->: Id(A)(a1)(a3) ))))))
  val a_eq_a3 = "_ : a=a3" :: Id(A)(a)(a3)
  val trans = ind_a1eqa2_a2eqa3_a1eqa3(A :~> (a :~> (a3 :~> (a_eq_a3 :-> a_eq_a3))))

  map !: A ~>: a1 ~>: a2 ~>: (Id(A)(a1)(a2) ->: B ~>: h ~>: Id(B)(h(a1))(h(a2)) )
  trans !: A ~>: a1 ~>: a2 ~>: (Id(A)(a1)(a2) ->: a3 ~>: (Id(A)(a2)(a3) ->: Id(A)(a1)(a3) ))

  val rec = "rec" :: C ~>: (C ->: (Nat ->: C ->: C) ->: Nat ->: C)
  val beta1 = "rec(c,f,0)=???" :: C ~>: c ~>: f ~>: Id(C)( rec(C)(c)(f)(zero) )(c)
  val beta2 = "rec(c,f,succ(n))=???" :: C ~>: c ~>: f ~>: n ~>: Id(C)( rec(C)(c)(f)(succ(n)) )(
    f(n)(rec(C)(c)(f)(n))
  )

  val recNN = rec(Nat)
  val double = recNN(zero)(n :-> (m :-> succ(succ(m)) ))

  val constSuccSucc = n :-> (m :-> succ(succ(m)))
  val double_0_eq_0 = beta1(Nat)(zero)(constSuccSucc) !: Id(Nat)( double(zero) )(zero)
  val double_1_eq_2 = trans(Nat)( double(one) )( succ(succ(double(zero))) )(
    beta2(Nat)(zero)(constSuccSucc)(zero)
  )(two)(
    map(Nat)( double(zero) )(zero)(double_0_eq_0)(Nat)( m :-> succ(succ(m)) )
  )

  val double_2_eq_4 = trans(Nat)( double(two) )( succ(succ(double(one))) )(
    beta2(Nat)(zero)(constSuccSucc)(one)
  )(four)(
    map(Nat)( double(one) )(two)(double_1_eq_2)(Nat)( m :-> succ(succ(m)) )
  )

  def main(args: Array[String]): Unit = {
    double_1_eq_2 !: Id(Nat)( double(one) )(two)
    double_2_eq_4 !: Id(Nat)( double(two) )(four)
  }
}
