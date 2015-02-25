(**

   Groups refine {!Monoid}s, in particular {!Monoid.Cancelative}
   monoids, augmenting cancelation to {i inversion}. In particular,
   finite bicancelative monoids are groups, but {!Group.S} is a
   more stringent signature to ascribe a structure to.

   {b Additional laws include:}

   - [ inv a ^ a = unit ] ({i left inverse})
   - [ a ^ inv a = unit ] ({i right inverse})

*)


module type S = sig
  include Monoid.S
  val inv : t -> t
end

module Infix (S : S) = struct
  include S
  let (&) = S.mult
end

(** Group multiplication is law abiding when "flipped". *)
module Flip (G : S) : S with type t = G.t = struct
  include G
  let mult a b = G.mult b a
end
