(**

   Applicative (covariant) functors are types which respect
   products (see {!Applicative.Monoidal.S} for more details). Laws
   will be described for both the {!Applicative.Monoidal.S}
   signature and the {!Applicative.S} signature individually.

*)


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
