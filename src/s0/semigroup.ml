(**

   Semigroups are types along with a single operation which must be
   associative.

   {b Laws include:}

   - [ a ^ (b ^ c) = (a ^ b) ^ c ] ({i associtivity})

*)

module type S = sig
  type t
  val mult : t -> t -> t
end

(** Masks string concatenation, but string concatenation is a
        Semigroup. *)
module Infix (S : S) = struct
  include S
  let (^) = S.mult
end

(** Semigroup multiplication is law abiding when "flipped". *)
module Flip (S : S) : S with type t = S.t = struct
  include S
  let mult a b = S.mult b a
end
