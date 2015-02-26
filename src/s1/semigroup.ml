(**

   Semigroups are types along with a single operation which must be
   associative.

   {b Laws include:}

   - [ a ^ (b ^ c) = (a ^ b) ^ c ] ({i associtivity})

*)

module type S = sig
  type 'a t
  val mult : 'a t -> 'a t -> 'a t
end

module Infix (S : S) = struct
  include S
  let (^) = S.mult
end

(** Semigroup multiplication is law abiding when "flipped". *)
module Flip (S : S) : S with type 'a t = 'a S.t = struct
  include S
  let mult a b = S.mult b a
end
