lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a->a
factorial 0 = 1
factorial n=n*factorial(n-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length2 :: Num a => [b] -> a
length2 b = sum[1 | _ <- b]

lengthRecursive :: (Num b) => [a] -> b
lengthRecursive [] = 0
lengthRecursive (_:xs) = 1 + lengthRecursive xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs)=x+sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!" 

max' :: (Ord a) => a -> a -> a  
max' a b 
    | a > b = a 
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!" 
    where bmi = weight / height ^ 2

cilinderArea :: (RealFloat a) => a -> a -> a
cilinderArea r h=
    let sideArea = 2 * pi * r *h
        topArea = 2 * pi * r * r
    in sideArea + topArea

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (elem:lista)=
    let smallerSorted = quickSort [a | a <- lista, a < elem]
        biggerSorted = quickSort [a | a<- lista, a >= elem]
    in smallerSorted ++ [elem] ++ biggerSorted

