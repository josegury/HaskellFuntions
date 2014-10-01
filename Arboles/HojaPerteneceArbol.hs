pertenece :: a -> ArbolG a -> Bool
pertenece a AVG = False 
pertenece a (AG r s) = if ( a == r) then true else (aux a s)

aux :: (Eq a) => a -> [ArbolG a] -> Bool
aux a [] = False;
aux a (h:t) if pertenece a h then True else aux a t

