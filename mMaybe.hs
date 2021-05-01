import Test.QuickCheck
{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

f1 :: String -> Maybe Int
f1 x = if length x > 2 then Just $ length x else Nothing

g1 :: Int -> Maybe String
g1 x = if x > 0 then Just $ concat $ replicate x "ab" else Nothing 

f2, f3, f4 :: Int -> Maybe Int
f2 a = if a >= 0 then Just a else Nothing
f3 a = if a < 10 then Just a else Nothing
f4 a = if a < 0 then Just (a * a) else Nothing

test1 = quickCheck (asoc f2 f3 f4)
-- +++ OK, passed 100 tests.

-- asta e extra asa
f5 x = do
    rezultat1 <- f2 x
    rezultat2 <- f3 rezultat1
    return rezultat2

asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = (h <=< (g <=< f) $ x) == ((h <=< g) <=< f $ x)

-- pos verifica doar daca un element e pozitiv sau nu
pos :: Int -> Bool
pos  x = if (x>=0) then True else False

-- foo verifica daca un element din context monadic este pozitiv, in cazul de fata din contextul Maybe 
-- de exemplu : foo (Just (-5)), Just (-5) e monada si extrage (-5)-ul din ea, apoi aplica functia pos pe (-5)
-- si rezulta tot o monada Maybe dar cu valoarea Just True sau Just False
foo :: Maybe Int ->  Maybe Bool 
foo  mx =  mx  >>= (\x -> Just (pos x))  

-- foo2 este foo cu do notation
foo2 :: Maybe Int ->  Maybe Bool 
foo2 mx = do
    x <- mx
    return (pos x)

-- foo2 $ Just 5
-- Just True

-- foo2 $ Just $ -5
-- Just False

testFoo mx = foo mx == foo2 mx
-- quickCheck testFoo
-- OK, pased 100 tests

-- cu do notation
addM :: Maybe Int -> Maybe Int -> Maybe Int  
addM mx my = do
    x <- mx
    y <- my
    return (x+y)
-- addM (Just 5) (Just 7)
-- Just 12

-- cu pattern matching
addM1 :: Maybe Int -> Maybe Int -> Maybe Int  
addM1 (Just x) (Just y) = Just (x+y)
addM1 _ _ = Nothing

-- inca o varianta smechera cu operatorul bind
addM3 :: Maybe Int -> Maybe Int -> Maybe Int  
addM3 mx my = mx >>= (\x -> my >>= (\y -> Just(x+y))) 

cartesian_product1 :: [a] -> [a] -> [(a, a)]
cartesian_product1 xs ys = do 
    x <- xs;
    y <- ys;
    return (x, y)

-- functie simpla pt testare
func x y = x + y
--functia prod facuta cu list comprehension si cu do notation
prod f xs ys = [ f x y | x <- xs, y <-ys]
prod_do f xs ys = do
    x <- xs
    y <- ys 
    return (f x y)   

--prod_do func (Just 3) (Just 2)
--Just 5
--prod_do func [1,2,3] [4,5]
--[5,6,6,7,7,8]

myGetLine:: IO String
myGetLine = do
    x <- getChar
    if x == '\n' then return []
    else do
        xs <- myGetLine
        return (x:xs)

myGetLine2 :: IO String
myGetLine2 = do
    x <- getChar
    if x == '\n' then return [] else do
        xs <- myGetLine
        return (x:xs)



