type ('a, 'b) product = 'a * 'b
type ('a, 'b) t = ('a, 'b) product

val bimap : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
val lmap : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val rmap : ('a -> 'b) -> 'c * 'a -> 'c * 'b
val fold : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  
module Left : sig
  type ('a, 'b) t = ('a, 'b) product
  val map : ('a -> 'b) -> 'a * 'c -> 'b * 'c
end

module Right : sig
  type ('a, 'b) t = ('a, 'b) product
  val map : ('a -> 'b) -> 'c * 'a -> 'c * 'b
end
