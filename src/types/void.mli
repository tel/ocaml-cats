(** The impossible type: no values can be made to exist.

    Values of type {!t} cannot be made to exist, there are no
    introduction forms. This makes them the impossible type; however,
    they are not without their use. A function which claims to return
    {!t} must actually never return. A hypothetical situation which
    offers a value of {!t} must be {!absurd}.

    Technically this should be the bottom type but there's no way to
    convince OCaml of this fact.

*)

type t = { absurd : 'a . 'a }
(** The nonexistent data type. *)

val absurd : t -> 'a
(** It is possible in some hypothetical contexts to have access to a
    value of type {!t}. As no values of {!t} can ever come into
    existence we resolve that our hypotheses are wrong and thus
    conclude whatever we like.

    In pithier words, {i from nothing comes anything}.
*)
