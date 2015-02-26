(**

   Bicontravariant types are those which are contravariant functors in
   each parameter.

   Bicontravariant types obey the contravariant functor laws in each
   type parameter.

*)

module type S = sig
  type (-'a, -'b) t
  val bicap : ('a -> 'a_) -> ('b -> 'b_) -> ('a_, 'b_) t -> ('a, 'b) t
end
