
type ('a, 'b) sum = Inl of 'a | Inr of 'b
type ('a, 'b) t = ('a, 'b) sum

val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) sum -> ('b, 'd) sum
val lmap : ('a -> 'b) -> ('a, 'c) sum -> ('b, 'c) sum
val rmap : ('a -> 'b) -> ('c, 'a) sum -> ('c, 'b) sum
val fold : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) sum -> 'b
  
module Left : sig
  (*

     I'd like to be able to just write the following include, but
     this introduces a circular dependency since we need Sum for the S2
     module. To fix this we could have a "core, bootstrap" types module
     which exports [sum] outside of its module and then reduce the dependency
     in S2.Profunctor.

     Alternatively, and perhaps better, we could change Sum to export
     polymorphic variants (an "even more public" definition) and define the
     profunctor type in terms of polymorphic variants.

     include S2.Monad.S with type ('e, 'a) t := ('e, 'a) t

  *)
  type ('a, 'b) t = ('a, 'b) sum
  val pure : 'a -> ('a, 'b) sum
  val map : ('a -> 'b) -> ('a, 'c) sum -> ('b, 'c) sum
  val ap : ('a -> 'b, 'c) sum -> ('a, 'c) sum -> ('b, 'c) sum
end

module Right : sig
  type ('a, 'b) t = ('a, 'b) sum
  val pure : 'a -> ('b, 'a) sum
  val map : ('a -> 'b) -> ('c, 'a) sum -> ('c, 'b) sum
  val ap : ('a, 'b -> 'c) sum -> ('a, 'b) sum -> ('a, 'c) sum
end

module Collect (X : S0.Monoid.S) : sig
  type +'a t = (X.t, 'a) sum
  include S1.Applicative.S with type 'a t := 'a t
end
