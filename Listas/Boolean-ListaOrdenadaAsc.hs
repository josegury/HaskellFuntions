data Lista a = Nill | Cost a (Lista a) deriving (Show, Eq, Ord)

ordenada :: (Ord a) => [Int] -> Bool

ordenada [] = error "La lista esta vacia"
ordenada [n] = True
ordenada (n:m:p) = if n<m then ordenada (m:p) else False
