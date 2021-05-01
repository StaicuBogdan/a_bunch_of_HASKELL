
--- Monada Writer
newtype WriterLS a = Writer { runWriter :: (a, [String]) } 

instance  Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)

instance  Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma 


tell :: String -> WriterLS () 
tell log = Writer ((), [log])

logIncrement :: Int -> WriterLS Int
logIncrement x = do
    tell ("incremented "++ show x)
    return (x+1)

logIncrement2 :: Int -> WriterLS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y
                  
logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x 1 = logIncrement x
logIncrementN x n = do
    y <- logIncrement x
    logIncrementN y (n-1)

-- cool stuff : runWriter $ return 5 >>= logIncrement >>= logIncrement2 
-- afiseaza (8, ["incremented 5","incremented 6","incremented 7"])

-- some more: runWriter $ return 5 >>= logIncrement >>= logIncrement2 >>= logIncrementN 3
-- afiseaza (11, ["incremented 5","incremented 6","incremented 7","incremented 8","incremented 9","incremented 10"])

-- asta functioneaza ca un:  map (\x -> if (x>=0) then True else False)
isPos :: Int -> WriterLS Bool
isPos x = if (x>= 0) then (Writer (True, ["poz"])) else (Writer (False, ["neg"]))                           

-- map isPos [1,2,3] da eroare pentru ca nu putem afisa pur si simplu rezultate monadice
-- map runWriter $ map isPos [1,-2,3] asa merge si afiseaza
-- [(True,["poz"]),(False,["neg"]),(True,["poz"])]

mapWriterLS :: (a -> WriterLS b) -> [a] -> WriterLS [b]
mapWriterLS f xs =  Writer ([a | (a,b) <- map runWriter $ map f xs], [head d | (c,d) <- map runWriter $ map f xs])





















