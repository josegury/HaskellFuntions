--dado un arbol binario, encontrar la rama mas larga

rlarga :: Arbol a -> [a]
rlarga (AB r i d) = if (numrama i) < (numrama d) 
			then r : rlarga d
			else r : rlarga i

numrama :: Arbol a -> Int
numrama AV = 0
numrama (AB r AV AV ) = 1
numrama (AB r i d ) = 1 + numrama i + numrama d
