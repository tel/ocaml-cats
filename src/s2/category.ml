(**

   Categories are "paths in directed graphs with compression" or
   "multi-sorted monoids". The canonical category is [Ml], the
   category of OCaml types and functions.

*)

open Types

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

  module Apply = struct
    module type S = sig
      include S
      val apply : (('a, 'b) t * 'a, 'b) t
    end
  end
end

(** The [Ml] category is the category of OCaml types and functions. *)
module Ml : sig
  type (-'a, +'b) t = 'a -> 'b

  include Profunctor.S        with type ('a, 'b) t := ('a, 'b) t
  include S                   with type ('a, 'b) t := ('a, 'b) t
  include Super.S             with type ('a, 'b) t := ('a, 'b) t
  include Profunctor.Strong.S with type ('a, 'b) t := ('a, 'b) t
  include Profunctor.Choice.S with type ('a, 'b) t := ('a, 'b) t
  include Arrow.Apply.S       with type ('a, 'b) t := ('a, 'b) t
end = struct
  type (-'a, +'b) t = 'a -> 'b

  let id x = x
  let ( @> ) f g = fun x -> g (f x)
  let ( <@ ) g f = fun x -> g (f x)

  let arrow f = f

  let dimap f g h = fun x -> g (h (f x))

  let first  f = Product.bimap f id
  let second f = Product.bimap id f
      
  let left  f = Sum.bimap f id
  let right f = Sum.bimap id f

  let apply (f, a) = f a
end

(** The Kleisli category of a monad [m] consists of the functions ['a
    -> 'b m]. *)
module Kleisli (M : S1.Monad.S) : sig
  type (-'a, +'b) t = 'a -> 'b M.t

  include Profunctor.S        with type ('a, 'b) t := ('a, 'b) t
  include S                   with type ('a, 'b) t := ('a, 'b) t
  include Super.S             with type ('a, 'b) t := ('a, 'b) t
  include Profunctor.Choice.S with type ('a, 'b) t := ('a, 'b) t
  include Arrow.Apply.S       with type ('a, 'b) t := ('a, 'b) t
end = struct
  type (-'a, +'b) t = 'a -> 'b M.t
      
  let id = M.pure
  let ( @> ) f g = fun a -> M.bind g @@ f a
  let ( <@ ) g f = fun a -> M.bind g @@ f a

  let arrow f = fun x -> M.pure @@ f x

  let dimap f g q = fun x -> M.map g @@ q @@ f x

  let first  f = fun (a, x) -> M.map (fun b -> (b, x)) @@ f a
  let second f = fun (x, a) -> M.map (fun b -> (x, b)) @@ f a
  
  let left  f = function
    | Sum.Inl a -> M.map Sum.Left.pure (f a)
    | Sum.Inr x -> M.pure (Sum.Inr x)
  let right f = function
    | Sum.Inr a -> M.map Sum.Right.pure (f a)
    | Sum.Inl x -> M.pure (Sum.Inl x)

  let apply (f, a) = f a
end

(** The Kleisli category of a comonad [w] consists of the functions
    ['a w -> 'b]. *)
module Cokleisli (W : S1.Comonad.S) : sig
  type (-'a, +'b) t = 'a W.t -> 'b

  include Profunctor.S        with type ('a, 'b) t := ('a, 'b) t
  include S                   with type ('a, 'b) t := ('a, 'b) t
  include Super.S             with type ('a, 'b) t := ('a, 'b) t
  include Profunctor.Choice.S with type ('a, 'b) t := ('a, 'b) t
  include Arrow.Apply.S       with type ('a, 'b) t := ('a, 'b) t
end = struct
  type (-'a, +'b) t = 'a W.t -> 'b
    
  let id = W.extract
  let ( @> ) f g = fun wa -> g @@ W.extend f wa
  let ( <@ ) g f = fun wa -> g @@ W.extend f wa

  let arrow f = fun wx -> f @@ W.extract wx

  let dimap f g q = fun wx -> g @@ q @@ W.map f wx

  let apply wfa =
    W.extract @@ W.map (fun (f, _) -> f @@ W.map snd wfa) wfa

  let first  f wax = (f (W.map fst wax), W.extract (W.map snd wax))
  let second f wxa = (W.extract (W.map fst wxa), f (W.map snd wxa))

  let coerceL = function
    | Sum.Inl a -> a
    | Sum.Inr _ -> failwith "Invariant broken---was not lefty"

  let coerceR = function
    | Sum.Inr a -> a
    | Sum.Inl _ -> failwith "Invariant broken---was not lefty"

  let left f wax = match W.extract wax with
    | Sum.Inr x -> Sum.Inr x
    | Sum.Inl _ -> Sum.Inl (f (W.map coerceL wax))

  let right f wxa = match W.extract wxa with
    | Sum.Inl x -> Sum.Inl x
    | Sum.Inr _ -> Sum.Inr (f (W.map coerceR wxa))
end
