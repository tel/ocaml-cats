
type ('a, 'b) sum = Inl of 'a | Inr of 'b
type ('a, 'b) t = ('a, 'b) sum

val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) sum -> ('b, 'd) sum
val lmap : ('a -> 'b) -> ('a, 'c) sum -> ('b, 'c) sum
val rmap : ('a -> 'b) -> ('c, 'a) sum -> ('c, 'b) sum
val fold : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) sum -> 'b
  
module Left : sig
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
