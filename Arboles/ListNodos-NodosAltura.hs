--dando la altura dar los nodos de esa altura
nivel :: Int -> Arbol a -> [a]
nivel n AV = []
nivel 1 (AB r _ _) = [r]
nivel n (AB r i d)) = (nivel (n-1) i) ++ (nivel (n-1) d)
