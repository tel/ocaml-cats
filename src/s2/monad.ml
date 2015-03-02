
(**

   Monadic covariant functors are types which can be used to
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


module type S = sig
  include Applicative.S

  val bind : ('a -> ('e, 'b) t) -> (('e, 'a) t -> ('e, 'b) t)
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
