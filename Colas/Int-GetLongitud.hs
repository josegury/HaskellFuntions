longitud :: Cola a -> Int
longitud CV = 0
longitud (c :. x) = 1 + longitud c
