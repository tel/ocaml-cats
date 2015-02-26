
type t = { absurd : 'a . 'a }
(* The absurdity of [t] is self-evident. The only "natural" way to
   produce a value of [t] is to have a value of literally any type
   with no constraint. *)

let absurd t = t.absurd
