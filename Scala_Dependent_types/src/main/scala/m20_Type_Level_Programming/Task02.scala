package m20_Type_Level_Programming

//noinspection DuplicatedCode,TypeAnnotation
object Task02 {
  sealed trait Nat {
    type This >: this.type <: Nat
    type ++ = Succ[This]
    type + [_ <: Nat] <: Nat
  }

  final object Zero extends Nat {
    type This = Zero
    type + [X <: Nat] = X
  }
  type Zero = Zero.type
  final class Succ[N <: Nat] extends Nat {
    type This = Succ[N]
    type + [X <: Nat] = Succ[N# + [X]]
  }
  type _0 = Zero
  type _1 = _0 # ++
  type _2 = _1 # ++
  type _3 = _2 # ++
  type _4 = _3 # ++
  type _5 = _4 # ++
  type _6 = _5 # ++
  type _7 = _6 # ++
  type _8 = _7 # ++
  type _9 = _8 # ++

  sealed trait Vec[n <: Nat, +A]
  object Nil extends Vec[_0, Nothing]
  type Nil = Nil.type
  class Cons[n <: Nat, A, a <: A, as <: Vec[n, A]] extends Vec[Succ[n], A]

  sealed trait Concat[n <: Nat, m <: Nat, A, vn <: Vec[n, A], vm <: Vec[m, A]] { type Res <: Vec[n# + [m], A] }

  implicit def concatNil[m <: Nat, A, vm <: Vec[m, A]] = new Concat[_0, m, A, Nil, vm] {
    type Res = vm
  }

  implicit def concatCons[n <: Nat, m <: Nat, A, a <: A, vn <: Vec[n, A], vm <: Vec[m, A], vnm <: Vec[n# + [m], A]]
      (implicit fact: Concat[n, m, A, vn, vm] { type Res = vnm }) =
    new Concat[Succ[n], m, A, Cons[n, A, a, vn], vm] {
      type Res = Cons[n# + [m], A, a, vnm]
    }

  trait A
  class a extends A
  class a1 extends A
  class a2 extends A
  class a3 extends A
  class a4 extends A
  type vec1 = Cons[_1, A, a, Cons[_0, A, a1, Nil]]
  type vec2 = Cons[_2, A, a2, Cons[_1, A, a3, Cons[_0, A, a4, Nil]]]
  type vec = Cons[_4, A, a, Cons[_3, A, a1, Cons[_2, A, a2, Cons[_1, A, a3, Cons[_0, A, a4, Nil]]]]]

  def main(args: Array[String]): Unit = {
    type t = Concat[_2, _3, A, vec1, vec2] { type Res = vec }
  }
}
