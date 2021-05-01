import Data.Monoid
import Control.Monad.Writer
--import Control.Monad.Instances

isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")

-- functie care aplica un log, o informatie extra la parametrul al doilea, fara sa piarda continutul actualului log
--applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  

--applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
--applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  

type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30) 
--exemplu
--("beans", Sum 10) `applyLog` addDrink  
--("milk",Sum {getSum = 35})

-- din momentul in care rezulta "beer", acel beer nu se mai schimba , doar mai adauga 30 la price
--("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink
--("beer",Sum {getSum = 65})

-- devine beer din prima, iar pretul creste cu 3*30 ca aplica pe beer de 3 ori
--("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink `applyLog` addDrink
--("beer",Sum {getSum = 95})

--devine whiskey mai intai, apoi beer, deci pretul creste cu 99 ,apoi cu 2*30
--("jerky", Sum 5) `applyLog` addDrink `applyLog` addDrink `applyLog` addDrink
--("beer",Sum {getSum = 164})



-- writer monad

-- multe nu merg :(
-- se pare ca atunci cand a fost scrisa LYAH , haskell era ceva mai diferit, eu am niste erori care acolo in carte nu sunt
-- exemplu : Control.Monad.Instances care zice ca e deprecated
-- si Control.Monad.Writer care efectiv nu merge


addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b) 


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  


data Direction = L | R deriving (Show)  
type Breadcrumbs = [Direction]


goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goLeft (Node _ l _, bs) = (l, L:bs) 

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goRight (Node _ _ r, bs) = (r, R:bs) 

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

goLeft2 :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goLeft2 (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight2 :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goRight2 (Node x l r, bs) = (r, RightCrumb x l:bs)


goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)  







