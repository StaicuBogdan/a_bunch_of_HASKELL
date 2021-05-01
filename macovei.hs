import Data.Char
import Data.List

import Test.QuickCheck
import Test.QuickCheck.Gen

---- lab 7

data Fruct
   = Mar String Bool
   | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden" True]


ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = elem soi ["Tarroco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia ((Mar _ _): fs) = nrFeliiSicilia fs
nrFeliiSicilia ((Portocala soi nr):fs)
    | ePortocalaDeSicilia (Portocala soi nr) = nr + nrFeliiSicilia fs
    | otherwise = nrFeliiSicilia fs

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi ((Portocala _ _): fs) =nrMereViermi fs
nrMereViermi ((Mar soi vierme):fs)
    | vierme == True = 1 + nrMereViermi fs
    | otherwise = nrMereViermi fs

data Linie = L [Int]
    deriving Show


data Matrice = M [Linie]

matrice = M[L[1,2,3,4],L[5,5],L[1,3,6]]

verifica :: Matrice -> Int -> Bool
verifica (M[]) _ = True
verifica (M (L x : xs)) n
    | sum x == n = verifica (M xs) n
    | otherwise = False

--------------------------------------------------------------------------------------------


double :: Int -> Int
double x = 2 * x

triple :: Int -> Int
triple x = 3 * x


penta :: Int -> Int
penta x = 5 * x

test x = (double x + triple x) == penta x
 
fail_test x = (double (double x)) == penta x

myLookUp :: Int -> [(Int, String)]-> Maybe String
myLookUp _ [] = Nothing
myLookUp element (x:xs)
    | element == fst x = Just (snd x)
    | otherwise = myLookUp element xs


myLookUpFoldr = foldr f i
    where
        i :: Int -> Maybe String
        i = \_ -> Nothing
        f :: (Int, String) -> (Int -> Maybe String) -> Int -> Maybe String
        f = \x u element -> if element == fst x then Just (snd x) else u element


myLookUpFold :: Int -> [(Int, String)]-> Maybe String
myLookUpFold element = foldr (\x found -> if fst x == element then Just (snd x) else found) Nothing

testMyLookUp element list = myLookUp element list == myLookUp element list

testMyLookUp2 element list = myLookUp element list == myLookUpFoldr list element



data ElemIS = I Int | S String
    deriving (Show, Eq)


myLookUpElem :: Int -> [(Int, ElemIS)]-> Maybe ElemIS
myLookUpElem _ [] = Nothing
myLookUpElem element (x:xs)
    | element == fst x = Just (snd x)
    | otherwise = myLookUpElem element xs

--instanta a clasei Arbitrary pentru tipul de date ElemIS


instance Arbitrary ElemIS where
    arbitrary = oneof [geni, gens]
        where
            f= (unGen (arbitrary :: Gen Int))
            g= (unGen (arbitrary :: Gen String))
            geni = MkGen (\s i -> let x = f s i in (I x))
            gens = MkGen (\s i -> let x = g s i in (S x))

testLookUpElem :: Int -> [(Int, ElemIS)] -> Bool
testLookUpElem element list = myLookUpElem element list == lookup element list



----------- lab 8

type Name = String
type Quantity = Int

data Ingredient = Ing Name Quantity
    deriving Show


data Reteta = R [Ingredient]
    deriving Show


r1= R [Ing "faina" 500, Ing "oua" 4, Ing "zahar" 500]
r2= R [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 4]
r3= R [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 55]


instance Eq Ingredient where
    (Ing name1 quantity1) == (Ing name2 quantity2) = map toLower name1 == map toLower name2 && quantity1 == quantity2

instance Ord Ingredient where
   (Ing n1 q1) <= (Ing n2 q2) = (map toLower n1) < (map toLower n2) || ((map toLower n1 == map toLower n2) && q1 <= q2)

eqReteta :: Reteta -> Reteta -> Bool
eqReteta (R []) (R []) = True
eqReteta _ (R []) = False
eqReteta (R []) _ = False
eqReteta (R ((Ing n1 q1) : ings1)) (R ((Ing n2 q2) : ings2)) 
    | length ings1 /= length ings2 = False
    | (Ing n1 q1) == (Ing n2 q2) = eqReteta (R ings1) (R ings2)
    | otherwise = False

instance Eq Reteta where
    (R r1) == (R r2) = eqReteta (R (sort r1)) (R (sort r2))




























