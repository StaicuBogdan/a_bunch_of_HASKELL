
--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)

logIncrement :: Int -> WriterS Int
logIncrement x = do
    tell ("increment: "++ show x ++ ", ")
    return (x+1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y
                  
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x 1 = logIncrement x
logIncrementN x n = do
    y <- logIncrement x
    logIncrementN y (n-1)

-- cool stuff : runWriter $ return 5 >>= logIncrement >>= logIncrement2 
-- afiseaza (8, "increment: 5, increment: 6, increment: 7, ")

-- some more: runWriter $ return 5 >>= logIncrement >>= logIncrement2 >>= logIncrementN 3
-- afiseaza (11, "increment: 5, increment: 6, increment: 7, increment: 8, increment: 9, increment: 10, ")









