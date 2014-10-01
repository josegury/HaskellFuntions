
data Lista a = Nill | Cost a (Lista a) deriving (Show, Eq, Ord)

insertar :: (Ord a) => a -> [a] -> [a]
insertar a [] = (a : [])
insertar a (h:t) = if (a<=h) then a:(h:t) else h:(insertar a t)

