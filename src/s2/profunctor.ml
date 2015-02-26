(**

   Profunctors are two-parameter data types such that the first
   parameter is contravariant and the second is covariant.

   Profunctors obey the contravariant functor laws in their first
   parameter and the covariant functor laws in their second parameter.

*)

open Types

module type S = sig
  type (-'a, +'b) t
  val dimap : ('a_ -> 'a) -> ('b -> 'b_) -> ('a, 'b) t -> ('a_, 'b_) t
end

module Strong = struct

  module type S = sig
    include S
    val first  : ('a, 'b) t -> ('a * 'x, 'b * 'x) t
    val second : ('a, 'b) t -> ('x * 'a, 'x * 'b) t
  end
  
end

module Choice = struct

  module type S = sig
    include S
    val left  : ('a, 'b) t -> (('a, 'x) Sum.t, ('b, 'x) Sum.t) t
    val right : ('a, 'b) t -> (('x, 'a) Sum.t, ('x, 'b) Sum.t) t
  end
  
end

module Costrong = struct

  module type S = sig
    include S
    val unfirst  : ('a * 'x, 'b * 'x) t -> ('a, 'b) t
    val unsecond : ('x * 'a, 'x * 'b) t -> ('a, 'b) t
  end
  
end

module Cochoice = struct

  module type S = sig
    include S
    val unleft  : (('a, 'x) Sum.t, ('b, 'x) Sum.t) t -> ('a, 'b) t
    val unright : (('x, 'a) Sum.t, ('x, 'b) Sum.t) t -> ('a, 'b) t
  end
  
end
