type (+'a, +'b) product = 'a * 'b
type (+'a, +'b) t = ('a, 'b) product
    
let bimap f g (a, b) = (f a, g b)
let lmap f = bimap f (fun x -> x)
let rmap f = bimap (fun x -> x) f
let fold f (a, b) = f a b
    
module Left = struct
  type (+'a, +'b) t = ('a, 'b) product
  let map f = lmap f
end

module Right = struct
  type (+'a, +'b) t = ('a, 'b) product
  let map f = rmap f
end
