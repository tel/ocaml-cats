(**

   Bicovariant types are those which are covariant functors in each
   parameter.

   Bicovariant types obey the covariant functor laws in each type
   parameter.

*)

module type S = sig
  type (+'a, +'b) t
  val bimap : ('a -> 'a_) -> ('b -> 'b_) -> ('a, 'b) t -> ('a_, 'b_) t
end
