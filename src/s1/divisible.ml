(**

   Divisible (contravariant) functors are dual to applicative
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

   where [f' a = let (bc, d) = f a in let (b, c) = g bc in (a, (b, c))
   ].

*)

module type S = sig
  include Contravariant.S
      
  val divide  : ('a -> 'l * 'r) -> 'l t -> 'r t -> 'a t
  val conquer : unit t
end
