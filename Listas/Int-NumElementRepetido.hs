data Lista a = Nill | Cost a (Lista a) deriving (Show, Eq, Ord)

veces :: (Eq a) a -> [a] -> Int
veces x [] = 0
vecces x (h:t) = if x == h then 1 + (veces x t) else veces x t



