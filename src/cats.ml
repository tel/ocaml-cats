
(** Cats: signatures in the cateogry theoretic style.

    This module contains little functionality but instead merely specs
    out a series of signatures useful for describing data types. The
    style of these signatures is directly inspired by work in Haskell
    and is called "in the category theoric style" due to the
    inspiration it takes in structure and naming conventions from the
    fields of abstract algebra and category theory.

    {1 Laws}

    Most signature come not only with signatures but also {i
    laws}. Laws are syntactic properties which relate the values in a
    signature together and bind them to a particular meaning. As an
    example, the signature {!S0.Semigroup} contains a type
    {!S0.Semigroup.t}, a member {!S0.Semigroup.mult}, and demands law
    abidance from structures ascribed to it in that if [x], [y], and
    [z] are of type {!S0.Semigroup.t} then {[ mult (mult x y) z ]} is
    identical to {[ mult x (mult y z) ]} ({i associativity}).

    The short story with laws is that if you write a functor taking a
    law-abiding signature as a argument then you are free to assume
    the laws hold while if you ascribe a module to one of these
    signatures then it is your onus to ensure the law is met.

    {1 Currying}

    In OCaml type constructors are never curried. This means that a
    signature for a type ['a t] which is {!S1.Covariant} is completely
    distinct from a signature for a type [('e, 'a) t] which is
    {!S2.Covariant} "in its second type parameter".

    Cats provides signatures as appropriate for type arities between 0
    and 2 in the corresponding modules {!S0}, {!S1}, and {!S2}.

*)

(** Signatures of monomorphic types. *)
module S0 = struct

  (** Semigroups are types along with a single operation which must be
      associative.
      
      {b Laws include:}
      
      - [ a ^ (b ^ c) = (a ^ b) ^ c ] ({i associtivity})
  *)
  module Semigroup = struct
    
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
    
  end
  
  (** Monoids refine {!Semigroup}s adding a unit element 
      
      {b Additional laws include:}
      
      - [unit ^ a = a] ({i left unit})
      - [a ^ unit = a] ({i right unit})
  *)
  module Monoid = struct
    
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
      
    (** Cancelative monoids are monoids which can be {i canceled} on
        one side or the other. *)
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
  
end

  (** Groups refine {!Monoid}s, in particular {!Monoid.Cancelative}
      monoids, augmenting cancelation to {i inversion}. In particular,
      finite bicancelative monoids are groups, but {!Group.S} is a
      more stringent signature to ascribe a structure to.

      {b Additional laws include:}

      - [ inv a ^ a = unit ] ({i left inverse})
      - [ a ^ inv a = unit ] ({i right inverse})

  *)
  module Group = struct

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
    
  end

end
