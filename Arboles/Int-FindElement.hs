-- dado un arbol binario encontrar el siguiente elemento de uno dado

sig :: Arbol Int -> Int -> Int
sig AV _ = []
sig (AB r i d) x = if (r <= x)
			then sig d x
			else if x > (maximo i)
				then r
				else sig i x
				


