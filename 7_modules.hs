import Data.List
import Data.Function
import Data.Char
--import Data.Map
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geom.Sphere as Sphere  
import qualified Geom.Cuboid as Cuboid  
import qualified Geom.Cube as Cube
import Geometry


-- daca vreau sa dau import la toata libraria fara o functie anume
-- import Data.List hiding (nub)

-- ca sa nu se incurce functiile intre ele
-- import qualified Data.Map as M 

uniqueNumbers :: (Eq a) => [a] -> Int
uniqueNumbers = length . nub

uniqueNumbers2 :: (Eq a) => [a] -> Int
uniqueNumbers2 = \xs -> length (nub xs)


-- de exemplu search "ab" "xabcd"
-- iti face tails de al doilea string care sunt {xabcd abcd bcd cd d]
-- apoi face functia lambda care verifica daca primele 2 litere din fiecare coada a lui xabcd sunt egale cu "ab"
-- daca sunt egale inseamna ca string-ul needle se regaseste in haystack , iar foldl va returna true

search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)


-- caesar cihper 

encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted

decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg


-- Data.Map

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]

-- gaseste valoarea unei chei
findKey :: (Eq k) => k -> [(k,v)] -> v  
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- asta ca sa nu dea crash cand nu exista valoarea in map 
findKey2 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey2 key [] = Nothing  
findKey2 key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey2 key xs

-- aici si cu foldr, caci e caz clasic de recursie
findKey3 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey3 key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing 

--Note: It's usually better to use folds for this standard list recursion pattern 
--instead of explicitly writing the recursion because they're easier to read and identify. 
--Everyone knows it's a fold when they see the foldr call, but it takes some more 
--thinking to read explicit recursion.

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- map fst . Map.toList . Map.insert 9 2 $ Map.singleton 4 3
--[4,9]

phoneBook2 =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs


-- Data.Set

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"








