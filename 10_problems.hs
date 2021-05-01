-- Reverse Polish notation calculator

solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs 

-- varianta extinsa cu alte operatii

solveRPN2 :: String -> Float  
solveRPN2 = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs

--ghci> solveRPN "10 4 3 + 2 * -"  
---4  
--ghci> solveRPN "2 3 +"  
--5  
--ghci> solveRPN "90 34 12 33 55 66 + * - +"  
---3947  
--ghci> solveRPN "90 34 12 33 55 66 + * - + -"  
--4037  
--ghci> solveRPN "90 34 12 33 55 66 + * - + -"  
--4037  
--ghci> solveRPN "90 3 -"  
--87 


-- Heathrow to London

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]

-- un drum ca cel din exemplu
heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]


data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]
--[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)] asta ar trb sa returneze , o lista de Paths

-- functia ar trebui sa arate asa:  optimalPath :: RoadSystem -> Path
-- va consta tot intr-un left fold , caci roadSystem este o lista 
-- pentru fiecare sectiune vom avea o verificare de genul (Path, Path) -> Section -> (Path, Path)
-- pentru a alfa care e cel mai scurt drum pana la urmatoarele 2 noduri
-- (Path, Path) -> Section -> (Path, Path) va putea fi folosita si ca functie binara la Left Fold

roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)

-- path-ul returnat va fi in ordina inversa deoarece fac append in fata listei mereu

optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath


-- sum $ map snd (optimalPath heathrowToLondon)
-- rezulta 75