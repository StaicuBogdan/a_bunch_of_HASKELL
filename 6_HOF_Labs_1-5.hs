
import Data.Char
import Data.List


compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

applyThreeTimes :: (x-> x) -> x -> x
applyThreeTimes f a = f (f (f a))

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z

multTwoWithNine :: Integer -> Integer -> Integer
multTwoWithNine = multThree 9

multWithThirtySix :: Integer -> Integer
multWithThirtySix = multTwoWithNine 4

divideByTwenty :: (Floating a) => a -> a
divideByTwenty = (/20)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' functie (x:xs) (y:ys) = functie x y : zipWith' functie xs ys

-- zipWith (^) (replicate 6 3) [1..]
-- >> [3,9,27,81,243,729]

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []  
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3827 == 0  

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)


numberOfChains :: Int
numberOfChains = length ( filter longerThanFifteen (map chain [1..100]) )
    where longerThanFifteen x = (length x > 15)

--variantae cu lambda 

numberOfChains' :: Int
numberOfChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))


-- tot cu lambda 

--zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
-- >> [153.0,61.5,31.0,15.75,6.6]

addTwo :: (Num a) => a -> a -> a
addTwo = \x -> \y -> x + y

--- foldl

sum' :: (Num a) => [a] -> a
sum' lista = foldl (\val x -> val + x) 0 lista

prod' :: (Num a) => [a] -> a
prod' lista = foldl (\val y -> val * y ) 1 lista

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []

reverse2 :: [a] -> [a]
reverse2 = foldl (flip (:)) []

--- foldr

map' :: (a->b) -> [a] -> [b]
map' functie lista = foldr (\x listaFinala -> functie x : listaFinala) [] lista


----------------------------- lab 2

fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n)
  where fibonacciPereche :: Integer -> (Integer, Integer)
        fibonacciPereche 1 = (0, 1)
        fibonacciPereche n = (x , x+y)
          where (y, x) = fibonacciPereche(n-1)

semiPare :: [Int] -> [Int]
semiPare [] = []
semiPare(x:xs)
    |even x =x`div`2 :(semiPare xs)
    |otherwise = semiPare xs


inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (x:xs)
    | x <= b && x >= a = x: inIntervalRec a b xs
    | otherwise = inIntervalRec a b xs

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b xs = [x | x<- xs,x <= b, x >= a]

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
    | x > 0 = 1 + pozitiveRec xs 
    | otherwise = pozitiveRec xs

pozitiveComp :: [Int] -> Int
pozitiveComp xs = sum [1 | _ <- xss]
    where xss = [x | x<- xs, x>0]


----------luni

functie :: [Int] -> Int -> Int
functie [] n = 0
functie xs n
    | n < 0 = 0
    | length [x | x<- xs , x>0, x<n] == 0 = 0
    | otherwise = maximum [x | x<- xs , x>0, x<n]

--pozitiiImpareRec :: [Int] -> [Int]
--pozitiiImpareRec xs
--    | null h = []
--    | h `div` 2 == 1 = pozitie : pozitiiImpareRec t
--    | otherwise = pozitiiImpareRec t
--          where h = head xs
--                t = tail xs
--                pozitie = fst 

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec xs = pozitiiImpareRecAux xs 0

pozitiiImpareRecAux [] _ = []
pozitiiImpareRecAux (x:xs) a
    | odd x = a : (pozitiiImpareRecAux xs (a+1))
    | otherwise = pozitiiImpareRecAux xs (a+1)


pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp xs = [fst a | a <- zip [0..] xs, snd a `mod` 2 == 1]


multDigitComp :: String -> Int
multDigitComp xs = product $ [ digitToInt a | a <- xs, isDigit a] ++ [1]

multDigitRec :: String -> Int
multDigitRec "" =  1
multDigitRec (x:xs)
    | isDigit x = digitToInt x * multDigitRec xs
    | otherwise = multDigitRec xs

discountRec :: [Float] -> [Float]
discountRec [] = []
discountRec (x:xs)
    | discount x < 200 = discount x : discountRec xs
    | otherwise = discountRec xs
      where discount = \y -> y - y * 0.25

discountComp :: [Float] -> [Float]
discountComp xs = [0.75 * x | x <- xs, 0.75 *x < 200]


-------------------------- LAB 3 --------------------------------------------


-- L3.1 Încercati sa gasiti valoarea expresiilor de mai jos si
-- verificati raspunsul gasit de voi în interpretor:
{-
[x^2 | x <- [1 .. 10], x `rem` 3 == 2]
[(x, y) | x <- [1 .. 5], y <- [x .. (x+2)]]
[(x, y) | x <- [1 .. 3], let k = x^2, y <- [1 .. k]]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A' .. 'Z']]
[[x .. y] | x <- [1 .. 5], y <- [1 .. 5], x < y ]

-}

factori :: Int -> [Int]
factori x = [ a | a <- [2..x`div`2], x `mod` a == 0]

prim :: Int -> Bool
prim x = factori x == []

numerePrime :: Int -> [Int]
numerePrime x = [ a | a <- [2..x], prim a]

-- L3.2 Testati si sesizati diferenta:
-- [(x,y) | x <- [1..5], y <- [1..3]]
-- zip [1..5] [1..3]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 _ _ [] = []
myzip3 _ [] _ = []
myzip3 [] _ _ = []
myzip3 (h1:t1) (h2:t2) (h3:t3) = (h1, h2, h3) : myzip3 t1 t2 t3

-- map ($ 3) [ (4 +), (10 *), (^3), sqrt]

---map
firstEl :: [(a,b)] -> [a]
firstEl = map (\x->fst x)

sumList :: [[Int]] -> [Int]
sumList = map (\x -> sum x) 

prel2 :: [Int] -> [Int]
prel2 = map (\x -> if even x then x`div`2 else x*2)

---filter
filterEx1 :: Char -> [String] -> [String]
filterEx1 c xs = filter (c `elem`) xs 

filterEx2 :: [Int] -> [Int]
filterEx2 xs = map (^2) $ filter odd xs

filterEx2' :: [Int] -> [Int]
filterEx2' xs = map (^2) (filter odd xs)

filterEx3 :: [Int] -> [Int] 
filterEx3 xs = map (\x -> snd x ^ 2) (filter (\x -> odd (fst x)) (zip [0..] xs))

filterEx4 :: [String] -> [String]
filterEx4 xs = map (filter (`elem` "aeiouAEIOU")) xs

------------------- LAB 4 -----------------------------

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and [ x<y | (x,y) <- zip (x:xs) xs]

ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True 
ordonataNat1 (h:t)
    | length t == 0 = True
    | h < head t && length t ==1 = True
    | h < head t = ordonataNat1 t
    | otherwise = False


ordonata :: [a] -> ( a-> a -> Bool ) -> Bool
ordonata [] _ = True 
ordonata (x:xs) f = and [ f a b | (a,b) <- zip (x:xs) xs]

-- niste exemple pt functia de mai sus 
-- ordonata [1,2,3,4,5,6,7] (==) 
-- False
-- ordonata [1,2,3,4,5,6,7] (<=)
-- True
-- ordonata [1,2,4,8,16] (\x y -> y `mod` x == 0)
-- True

(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(*<*) (x,y) (z,w) =  and [x<z, y<w]

-- ordonata [(3,2),(4,3)] (*<*) da True

compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList f listaFunctii = map (f.) listaFunctii

aplicaList :: a -> [(a -> b)] -> [b]
aplicaList x xs = map ($ x) xs

myzip32 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip32 l1 l2 l3 = map (\((x,y),z) -> (x,y,z)) (zip (zip l1 l2) l3)


---- foldr

produsRec :: [Integer] -> Integer
produsRec [] = 1
produsRec (x:xs) = x * produsRec xs 

produsComp :: [Integer] -> Integer
produsComp (x:xs) = foldr (\x acc -> x * acc) 1 xs
--produsComp (x:xs) = foldr (*) 1 xs


andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs)
    | x == True = andRec xs
    | otherwise = False

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

rmChar :: Char -> String -> String
rmChar cat sir = filter ( cat /= ) sir

rmCharsRec :: String -> String-> String
rmCharsRec [] xs2 = xs2
rmCharsRec (x1:xs1) xs2 = rmCharsRec xs1 (rmChar x1 xs2)

testRmChars :: Bool
testRmChars = rmCharsRec ['a'..'l'] "fotbal" == "ot"

rmCharsFold :: String -> String -> String
rmCharsFold cuv1 cuv2 = foldr rmChar cuv2 cuv1 




















