(**

   Categories are "paths in directed graphs with compression" or
   "multi-sorted monoids". The canonical category is [Ml], the
   category of OCaml types and functions.

*)

module type S = sig
  type (-'a, +'b) t
  val id : ('a, 'a) t
  val ( @> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( <@ ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end

(** A [Super] category is one in which all of [Ml] can be injected via
    [arrow]. *)
module Super = struct

  module type S = sig
    include S
    val arrow : ('a -> 'b) -> ('a, 'b) t
    include Profunctor.S with type ('a, 'b) t := ('a, 'b) t
  end
  
end

(** Arrows are supercategories which are also strong profunctors. *)
module Arrow = struct

  module type S = sig
    include S
    val arrow : ('a -> 'b) -> ('a, 'b) t
    include Profunctor.Strong.S with type ('a, 'b) t := ('a, 'b) t
  end
  
end
