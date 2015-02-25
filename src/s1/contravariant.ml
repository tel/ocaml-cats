(**
   Contravariant functors are data types which can be "capped
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

module type S = sig
  type 'a t
  val cap : ('a -> 'b) -> ('b t -> 'a t)
end
