type (+'a, +'b) sum = Inl of 'a | Inr of 'b
type (+'a, +'b) t = ('a, 'b) sum
    
let bimap f g = function
  | Inl a -> Inl (f a)
  | Inr b -> Inr (g b)
               
let lmap f = bimap f (fun x -> x)
let rmap f = bimap (fun x -> x) f
    
let fold f g = function
  | Inl a -> f a
  | Inr b -> g b
               
module Left = struct
  type (+'a, +'b) t = ('a, 'b) sum
  let pure a = Inl a
  let map f = lmap f
  let ap ef ex = match ef, ex with
    | Inl ef, Inl ex -> Inl (ef ex)
    | Inr x,  _      -> Inr x
    | _,      Inr x  -> Inr x
end

module Right = struct
  type (+'a, +'b) t = ('a, 'b) sum
  let pure a = Inr a
  let map f = rmap f
  let ap ef ex = match ef, ex with
    | Inr ef, Inr ex -> Inr (ef ex)
    | Inl x,  _      -> Inl x
    | _,      Inl x  -> Inl x
end
