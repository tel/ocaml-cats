
(** A near-semiring (or {i seminearring}) is a pair of two {!
    Monoid}s glued together in lawful coherence. Conventionally one
    monoid is considered "additive" and the other "multiplicative" and
    the names and syntax support this convention.

    In particular, a near-semiring is distributive only on one side.

    {b Laws include:}

    - [a + (b + c) = (a + b) + c]
    - [a + zero = a]
    - [zero + a = a]

    - [a * (b * c) = (a * b) * c]
    - [a * one = a]
    - [one * a = a]

*)

module NearSemiring = struct
end
