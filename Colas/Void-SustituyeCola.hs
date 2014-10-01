sustituyecola :: (Eq) => a -> a -> Cola a -> Cola a
sustituyecola _ _ CV = CV
sustituyecola x y (c :. h) = if h==x then (sustituyecola x y c) :. y else (sustituyecola x y c) :. h 
