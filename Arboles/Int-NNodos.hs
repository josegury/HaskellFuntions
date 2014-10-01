nNodos :: ArbolG a -> Int
nNodos AVG = 0;
nNodos (AG r s) = 1 + (naux s)

naux :: [ArbolG a] -> Int
naux [] = 0;
naux (h:t)= (nNodos h) + (naux t)
