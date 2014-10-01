hojas :: Arbol a -> [a]
hojas AV = []
hojas (AB r Av AV) = [r]
hojas (AB r i d) ) = hojas i ++ jojas d
