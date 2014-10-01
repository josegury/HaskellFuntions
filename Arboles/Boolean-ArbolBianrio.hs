binario (AG r []) = True
binario (AG r [c]) = binario c
binario (AG r [x, y]) = (bianrio x) && (binario y)
binario _ = False
