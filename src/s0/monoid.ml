(**

   Monoids refine {!Semigroup}s adding a unit element.

   {b Additional laws include:}

   - [unit ^ a = a] ({i left unit})
   - [a ^ unit = a] ({i right unit})

*)

module type S = sig
  include Semigroup.S
  val unit : t
end

(** Masks string concatenation, but string concatenation is a
    Monoid. *)
module Infix (M : S) = struct
  include M
  let (^) = M.mult
end 

(** Monoid multiplication is law abiding when "flipped". *)
module Flip (M : S) : S = struct
  include M
  let mult a b = M.mult b a
end

(** Commutative monoids are monoids which have an additional law that
    monoid multiplication commutes.

    {i Additional laws include: }

    - [a ^ b = b ^ a] ({i commutativity })

    {i Note:} this functor is merely the identity functor.

*)
module Commutative (M : S) : S = M
  
(** Cancelative monoids are monoids which can be {i canceled} on one
    side or the other. *)
module Cancelative = struct
  
  (** Left cancelative monoids are monoids which can be {i canceled} on
      the left side.
      
      {i Additional laws include:}
      
      - [ a ^/ (a ^ b) = b ] ({i left cancellation})
      
  *)
  module Left = struct
    
    module type S = sig
      include S
        
      (** [cancelLeft (a <> b) a = b] *)
      val cancelLeft : t -> t -> t
    end
    
    module Infix (M : S) = struct
      include M
      let ( ^ )  a b = M.mult a b
      let ( ^/ ) a b = cancelLeft b a
    end
    
  end
  
  (** Right cancelative monoids are monoids which can be {i
      canceled} on the right side.
      
      {i Additional laws include:}
      
      - [ (a ^ b) /^ b = a ] ({i right cancellation})
      
  *)
  module Right = struct
    
    module type S = sig
      include S
        
      (** [cancelRight (a <> b) b = a] *)
      val cancelRight : t -> t -> t
    end
    
    module Infix (M : S) = struct
      include M
      let ( ^ )  a b = M.mult a b
      let ( /^ ) a b = cancelRight a b
    end
    
  end
  
  (** Bicancelative monoids, or just "cancelative monoids" are
          monoids which can be {i canceled} on the left and right
          sides.
      
          {i Additional laws include:}
      
          - [ a ^/ (a ^ b) = b ] ({i left cancellation})
          - [ (a ^ b) /^ b = a ] ({i right cancellation})
      
  *)
  module Bi = struct
    
    module type S = sig
      include S
      include Left.S  with type t := t
      include Right.S with type t := t
    end
    
    module Infix (M : S) = struct
      include M
      let ( ^ )  a b = M.mult a b
      let ( ^/ ) a b = cancelLeft  b a
      let ( /^ ) a b = cancelRight a b
    end
    
  end
  
end
