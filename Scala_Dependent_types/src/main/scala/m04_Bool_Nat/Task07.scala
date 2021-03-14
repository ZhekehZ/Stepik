package m04_Bool_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task07 {
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

  val C = "C" :: Nat ->: Type
  val c  = "c" :: C(zero)
  val f = "f" :: n ~>: (C(n) ->: C(succ(n)) )

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

  val ind = "ind" :: C ~>: (C(zero) ->: (n ~>: (C(n) ->: C(succ(n)) )) ->: n ~>: C(n) )
  val beta1 = "ind(c,f,0)=???" :: C ~>: c ~>: f ~>: Id(C(zero))(ind(C)(c)(f)(zero))(c)
  val beta2 = "ind(c,f,succ(n))=???" :: C ~>: c ~>: f ~>: n ~>: Id(C(succ(n)))(ind(C)(c)(f)(succ(n)))(
    f(n)(ind(C)(c)(f)(n)))

  val constNat = n :-> Nat
  val recNN = ind(constNat)
  val double = recNN(zero)(n :-> (m :-> succ(succ(m)) ))

  val recNN0 = NatInd.rec(Nat)
  val double0 = recNN0(zero)(n :-> (m :-> succ(succ(m)) ))

  val constSuccSucc = n :-> (m :-> succ(succ(m)))
  val double_0_eq_0 = beta1(constNat)(zero)(constSuccSucc) !: Id(Nat)( double(zero) )(zero)

  val statement = n ~>: Id(Nat)(double(n))(double0(n))
  val indStatement = ind(n :-> Id(Nat)(double(n))(double0(n)))
  val hyp = "hyp" :: Id(Nat)(double(n))(double0(n))
  val proof = indStatement(double_0_eq_0)(n :~> (hyp :->
    trans(Nat)( double(succ(n)) )( succ(succ(double(n))) )(
      beta2(constNat)(zero)(constSuccSucc)(n)
    )(succ(succ(double0(n))))(
      map(Nat)(double(n))(double0(n))(hyp)(Nat)( m :-> succ(succ(m)) )
    )
  ))

  def main(args: Array[String]): Unit = {
    proof !: statement
  }
}
