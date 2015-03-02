
(**

   Covariant functors are data types which can be "mapped over". While
   containers holding values are common examples, the concept is
   significantly more general.

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
  type (+'e, +'a) t
  val map : ('a -> 'b) -> (('e, 'a) t -> ('e, 'b) t)
end
