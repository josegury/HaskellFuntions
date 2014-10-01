data Nat = Cero | Suc Nat deriving Show

suma :: NAt -> Nat -> Nat
suma Cero n = n 
suma (Suc m) n = Suc (suma n m )


--Ejemplo de declaracion del TAD booleano con generadores no libres

data BoolT = T | Not BoolT

-- convertir la cadena que se manda a solo T o (Not T)
fnBoolT :: BoolT -> BoolT
fnBoolT (Not (Not n)) = fnBoolT n
fnBoolT (Not T) = Not T
fnBoolT T = T

-- funcion Not

notT, notAux :: BoolT -> BoolT
notT b = notAux (fnBoolT b)

notAux = Not T
notAux (Not T) = T

-- funcion and

andT,andAux :: BoolT -> BoolT -> BoolT
andT x y = andAux ((fnBoolt x) (fnBoolt y))

andAux T T = T
andAux x y = Not T

-- funcion or

orT, orAux :: BoolT -> BoolT -> BoolT

orT x y = orAux ((fnBoolt x) (fnBoolt y))

orT (Not T) (Not T) = (Not T)
orT x y = T

-- funcion xor

xorT, xorAux :: BoolT -> BoolT -> BoolT

xorT x y = xorAux ((fnBoolt x) (fnBoolt y))

xorT (Not T) (Not T) = (Not T)
xorT T T = (Not T)
xorT x y = T

-- funcion par

par :: Nat -> Bool
par Cero = True
par (Suc Cero) = False
par (Suc (Suc n)) = par n 

--funcion igual

igual :: Nat -> Nat -> Bool
igual Cero (Suc n) = False
igual(Suc n) Cero = False
igual Cero Cero = True
igual (Suc m) (Suc n) = igual m n

-- funcion menor

menor :: Nat -> Nat -> Bool
menor Cero (Suc n) = True
menor (Suc n) Cero = False
menor Cero Cero = False
menor (Suc m) (Suc n) = menor m n

--funcion resta

resta :: Nat -> Nat -> Nat 
resta n Cero = n
resta Cero n = Cero
resta (Suc n) (Suc m) = resta n m

--funcion division

division :: Nat -> Nat -> Nat
division n m = if (menor n m) then Cero else Suc (division (resta n m) m)
	
--funcion logaritmo

loga :: Nat -> Nat
loga Cero = error"Error el Log de 0 no existe"
loga (Suc Cero) = Cero
loga (Suc (Suc Cero)) = Suc Cero
loga n = suma (Suc Cero) (loga (division n (Suc (Suc Cero))))

toNat :: Int -> Nat
toNat 0 = Cero
toNat n = Suc (toNat (n-1))
