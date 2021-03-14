package m20_Type_Level_Programming

object Task01 {
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

  sealed trait Nat {
    type This >: this.type <: Nat
    type ++ = Succ[This]
    type + [_ <: Nat] <: Nat
    type * [_ <: Nat] <: Nat
    type ^ [X <: Nat] = X# ^^ [This]

    type ^^ [_ <: Nat] <: Nat
  }

  object Zero extends Nat {
    type This = Zero
    type + [X <: Nat] = X
    type * [_ <: Nat] = Zero

    type ^^ [X <: Nat] = _1
  }
  type Zero = Zero.type
  class Succ[N <: Nat] extends Nat {
    type This = Succ[N]
    type + [X <: Nat] = Succ[N# + [X]]
    type * [X <: Nat] = (N# * [X])# + [X]

    type ^^ [X <: Nat] = (N# ^^ [X])# * [X]
  }

  def main(args: Array[String]): Unit = {
    implicitly[_2# ^ [_2] =:= _4]
  }
}
