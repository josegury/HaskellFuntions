data Lista a = Nill | Cost a (Lista a) deriving (Show, Eq, Ord)

--Lista de numeros consecutivos

consecutivos :: (Eq a) => a -> a -> [a] -> Bool
consecutivos _ _ [] = False
consecutivos _ _ [a] = False
consecutivios x y (n:m:p) = ((x==n) && (y==m)) || ((y==n) && (x==m)) || consecutivos x y (m:p) -- n , m, p-> n = 1º m = 2º p = RESTO


