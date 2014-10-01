-- Elimina todos los elementos x que estan en s.

quitar :: (Eq a) a -> LSR a -> LSR a
quitar _ Nada = Nada
quitar x (Pon y s) = if x==y then quitar x s else Pon y (quitar x s)
