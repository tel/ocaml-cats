
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


(** Signatures of types with a single parameter *)
module S1 = struct

  (** Covariant functors are data types which can be "mapped
      over". While containers holding values are common examples, the
      concept is significantly more general.

      Functor here should not be confused with functor in ML parlance,
      a mapping between modules. They both arise from a similar
      category theoretic notion, but {!Covariant} is not an ML
      functor.

      {b Laws include the "functor laws": }

      - [map id = id] ({i preservation of identity})
      - [map f (map g a) = map (fun x -> f (g x)) a] ({i preservation of composition})

      where [val id : 'a -> 'a] is defined [let id x = x].

  *)
  module Covariant = struct

    module type S = sig
      type 'a t
      val map : ('a -> 'b) -> ('a t -> 'b t)
    end

  end

  (** Applicative (covariant) functors are types which respect
      products (see {!Applicative.Monoidal.S} for more details). Laws
      will be described for both the {!Applicative.Monoidal.S}
      signature and the {!Applicative.S} signature individually. *)
  module Applicative = struct

    (** The "standard" applicative functor signature. The intention of
        these laws is to suggest that any applicative can be
        factored into a ordered linear sequence

        {[ pure f <*> a <*> b <*> c <*> d <*> ... ]}

        for some "atomic" applicative values ([a], [b], [c], [d],
        ...). As a special case, if [a] is itself "atomic" then this
        is trivial as [a = pure id <*> a] by the first law.

        - [pure id <*> a = a] ({i identity})
        - [pure compose <*> u <*> v <*> w = u <*> (v <*> w)] ({i composition})
        - [pure f <*> pure x = pure (f x)] ({i homomorphism})
        - [u <*> pure y = pure (fun f -> f y) <*> u] ({i interchange})

        For another interpretation of these laws see {!Monoidal.S}.

    *)
    module type S = sig
      type 'a t
      include Covariant.S with type 'a t := 'a t

      val pure : 'a -> 'a t
      val ap   : ('a -> 'b) t -> ('a t -> 'b t)
    end

    module Infix (A : S) = struct
      include A
      let (<*>) = A.ap
      let (<$>) = A.map
    end
    
    (** The "monoidal" applicative functor signature. This signature
        is less directly useful, but its equivalence with {!S} and
        more "obvious" law structure makes it useful.

        The intention of these laws is to suggest that pairs can be
        constructed "through" the applicative and then that the
        pairing function is a kind of {!Monoid}.

        - [map assoc (a ^ (b ^ c)) = (a ^ b) ^ c] ({i associativity})
        - [map fst (a ^ unit) = a] ({i right identity})
        - [map snd (unit ^ a) = a] ({i left identity})
        - [map (both f g) (a ^ b) = map f a ^ map g b] ({i naturality})

        Where [val assoc : ('a * ('b * 'c)) -> (('a * 'b) * 'c)] such
        that [let assoc (a, (b, c)) = ((a, b), c)] and [val both :
        ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('a1 * 'b1) -> ('a2 -> 'b2)]
        such that [let both f g (a, b) = (f a, g b)].

        See also: {{: http://strictlypositive.org/IdiomLite.pdf} {i
        Applicative programming with effects}} by McBride and
        Patterson

    *)
    module Monoidal = struct
      module type S = sig
        type 'a t
        include Covariant.S with type 'a t := 'a t
            
        val unit : unit t
        val mult : 'a t -> 'b t -> ('a * 'b) t
      end

      module Infix (M : S) = struct
        include M
        let (^) = M.mult
      end
    end
  end

  (** Monadic covariant functors are types which can be used to
      represent binding in abstract contexts making them highly
      suitable for embedded DSLs, especially those which capture side
      effecting regions of code.

      Laws include:

      - [ pure a >>= f = f a ] ({i left identity})
      - [ m >>= pure = m ] ({i right identity})
      - [ (m >>= f) >>= g = m >>= fun x -> f x >>= g ] ({i associativity})

      Another, perhaps more obvious formulation of these laws is that
      they're the category laws of the {!Category.Kleisli} category of
      the monad. In this form, the laws become

      - [ pure >=> f = f ] ({i left identity})
      - [ f >=> pure = f ] ({i right identity})
      - [ f >=> (g >=> h) = (f >=> g) >=> h ] ({i associativity})

  *)
  module Monad = struct

    module type S = sig
      type 'a t
      include Covariant.S with type 'a t := 'a t
      include Applicative.S with type 'a t := 'a t
      
      val bind : ('a -> 'b t) -> ('a t -> 'b t)
    end

    module Infix (M : S) = struct
      include M
      let (<*>) = M.ap
      let (<$>) = M.map
      let (>>=) m k = M.bind k m
      let (=<<) k m = M.bind k m
      let (<<) m2 m1 = m1 >>= fun _ -> m2
      let (>>) m1 m2 = m1 >>= fun _ -> m2
      let (>=>) f g = fun x -> f x >>= g
      let (<=<) g f = fun x -> f x >>= g
    end

  end

  (** Comonad covariant functors are types which represent values in
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
  module Comonad = struct
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
  end
  
  (** Contravariant functors are data types which can be "capped
      upon". Natural examples include predicates (['a t = 'a -> bool])
      and comparators (['a t = 'a -> 'a -> int]). Capping a
      contravariant functor with a function ['a -> 'b] transforms its
      type in the "opposite" direction like ['b t -> 'a t].

      Functor here should not be confused with functor in ML parlance,
      a mapping between modules. They both arise from a similar
      category theoretic notion, but {!Covariant} is not an ML
      functor.

      {b Laws include the "functor laws": }

      - [map id = id] ({i preservation of identity})
      - [map f (map g a) = map (fun x -> f (g x)) a] ({i preservation of composition})

      where [val id : 'a -> 'a] is defined [let id x = x].

  *)
  module Contravariant = struct

    module type S = sig
      type 'a t
      val cap : ('a -> 'b) -> ('b t -> 'a t)
    end
    
  end

  (** Divisible (contravariant) functors are dual to applicative
      covariant functors. The are types which respect division into
      pieces.

      A simplified set of laws work over [ let delta x = (x , x) ]:

      - [ m >< conquer = m ] ({i left unit })
      - [ conquer >< m = m ] ({i right unit })
      - [ (m >< n) >< o = m >< (n >< o) ] ({i associativity })

      where [ let ( >< ) x y = divide delta x y ]. A more
      sophisticated set of laws can describe [ divide f ] for more
      general values [f], [g]

      - [ m >< conquer = cap (fun x -> fst (f x)) m ]
      - [ conquer >< m = cap (fun x -> snd (f x)) m ]
      - [ divide g m n >< o = divide f' m (divide id n o) ]

      where [f' a = let (bc, d) = f a in let (b, c) = g bc in (a, (b,
      c)) ].

  *)
  module Divisible = struct

    module type S = sig
      type 'a t
      include Contravariant.S with type 'a t := 'a t

      val divide  : ('a -> 'l * 'r) -> 'l t -> 'r t -> 'a t
      val conquer : unit t
    end

  end
  
end
