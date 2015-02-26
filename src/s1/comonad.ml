(**

   Comonad covariant functors are types which represent values in
   context.

   Laws include:

   - [ w =>> extract = w ] ({i left identity })
   - [ extract (w =>> f)  = f w ] ({i right identity })
   - [ (w =>> g) =>> f = w =>> fun x -> f (x =>> g) ] ({i associativity })

   Another, perhaps more obvious formulation of these laws is that
   they're the category laws of the {!Category.CoKleisli} category
   of the comonad. In this form, the laws become

   - [ extract =>= f = f ] ({i left identity})
   - [ f =>= extract = f ] ({i right identity})
   - [ f =>= (g =>= h) = (f =>= g) =>= h ] ({i associativity})

*)

module type S = sig
  type 'a t
  include Covariant.S with type 'a t := 'a t
      
  val extract : 'a t -> 'a
  val extend : ('a t -> 'b) -> ('a t -> 'b t)
end

module Infix (C : S) = struct
  include C
  let (<$>) = C.map
  let (=>>) w k = C.extend k w
  let (<<=) k w = C.extend k w
  let (=>=) f g = fun x -> g (extend f x)
  let (=<=) g f = fun x -> g (extend f x)
end
