ordenadacola :: (ord a) => Cola a -> Bool
ordenadacola CV = True;
ordenadacola (CV :. x) = True
ordenadacola (c :. y :. x) = if y<=x then ordenadacola (c :. y) else False
