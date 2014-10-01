perteneceb :: a -> ArbolG a -> Bool
perteneceb a AVG = False 
perteneceb a (AG r i d) = if (a == r  || (perteneceb a i) || (perteneceb a d)
