import Data.Char
import Data.List
import Data.Maybe

import Test.QuickCheck
import Test.QuickCheck.Gen


--------------------lab 6-----------------------------

data Fruct = Mar String Bool | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10

listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True]

-- mini exemplu
--data CharInt 
--    = I Int 
--    | C Char

--list :: [CharInt]
--list = [C 'a', I 2, C 'b', C 'z', I 13, I 27]

-- ex 1
-- doar verific ce zice numele functiei
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = soi `elem` ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

-- ex 2
-- numarul de felii de portocala de sicilia dintr-o lista de fructe
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia ((Mar _ _): fs) = nrFeliiSicilia fs
nrFeliiSicilia ((Portocala soi nr):fs)
    | ePortocalaDeSicilia (Portocala soi nr) = nr + nrFeliiSicilia fs
    | otherwise = nrFeliiSicilia fs

-- ex 3
-- nr de mere cu viermi dintr-o lista de fructe
nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi ((Portocala _ _):fs) = nrMereViermi fs
nrMereViermi ((Mar soi vierme):fs)
    | vierme == True = 1 + nrMereViermi fs
    | otherwise = nrMereViermi fs


data Linie = L [Int]
    deriving Show

data Matrice = M [Linie]

matrice = M [ L [7,2,3,8], L [8,5,7], L[2,1,9,8]]

-- functie verifica care verifica daca suma elementelor de pe o linie este egala cu o valoare n

verifica :: Matrice -> Int -> Bool
verifica (M []) _ = True
verifica (M (L x:xs)) n
    | sum x == n = verifica (M xs) n
    | otherwise = False




------------ lab 7  -------------------------------------------




double :: Int -> Int
double x = 2 * x

triple :: Int -> Int
triple x = 3 * x 

penta :: Int -> Int
penta x = 5 * x

test x = (double x + triple x) == penta x


fail_test :: Int -> Bool
fail_test x = (double x + triple x + x ) == penta x


myLookUp :: Int -> [(Int, String)] -> Maybe String
myLookUp _ [] = Nothing
myLookUp element (x:xs)
    | element == fst x = Just (snd x)
    | otherwise        = myLookUp element xs

-- merge si fara xs dar stiam deja 
myLookUp2 :: Int -> [(Int, String)] -> Maybe String
myLookUp2 element xs = foldr (\x acc -> if fst x == element then Just (snd x) else acc) Nothing xs

-- proprietatea de universalitate 
-- g :: [(Int, String)] -> Int -> Maybe String
-- g [] = \_ -> Nothing
-- g (x:xs) el = if el== fst xs then Just (snd x) else g xs el

-- devine asta in cod, 
--g :: [(Int, String)] -> Int -> Maybe String
--g [] = \_ -> Nothing
--f x u element = if element == fst x then Just (snd x) else u element

-- asta e algoritmul scris desfasurat, cu tot cu tipurile de date la fiecare variabila, e practic foldr-ul
myLookUpFoldr = foldr f i
    where
        i :: Int -> Maybe String
        i = \_ -> Nothing
        f :: (Int, String) -> (Int -> Maybe String) -> Int -> Maybe String
        f = \x u element -> if element == fst x then Just (snd x) else u element


testMyLookUp element list = myLookUp element list == myLookUp2 element list

testMyLookUp2 element list = myLookUp element list == myLookUpFoldr list element 

-- merg ambele, si myLookUp, si myLookUp2 care e facuta cu foldr mai sus 
testLookUp :: Int -> [(Int, String)] -> Bool
testLookUp elem list = myLookUp2 elem list == lookup elem list

-- quickCheck cu constrangeri

-- asta intoarce o proprietate ce conditioneaza pe ce tipuri de date pot aplica functia testLookUp
testLookUpCond :: Int -> [(Int, String)] -> Property 
testLookUpCond element list = element > 0 && div element 5 == 0 ==> testLookUp element list

-- o functie myLookUp' care cand gaseste valoarea, ii face prima litera mare

-- la linia 133 , in loc de Just ((toUpper . head . snd $ x) : (tail . snd $ x))
-- putea fi Just (toUpper(head(snd x)):tail(snd x)), dar am facut eu cu function composition
myLookUp' :: Int -> [(Int, String)] -> Maybe String
myLookUp' _ [] = Nothing
myLookUp' element (x:xs)
    | element == fst x = Just ((toUpper . head . snd $ x) : (tail . snd $ x))
    | otherwise        = myLookUp' element xs

-- myLookUp' 2 [(1,"abc"),(2,"bcd"),(3,"ghi")]

-- sa se testeze ca myLookUp' este == cu lookup pentru listele care contin valori care incep cu majuscula

testMyLookUp' :: Int -> [(Int, String)] -> Property
testMyLookUp' element list = all (\x -> snd x /= "" && (isUpper . head . snd $ x)) list ==> myLookUp' element list == lookup element list  


data ElemIS = I Int | S String
    deriving (Eq, Show)

myLookUpElem :: Int -> [(Int, ElemIS)] -> Maybe ElemIS
myLookUpElem _ [] = Nothing
myLookUpElem element (x:xs)
    | element == fst x = Just (snd x)
    | otherwise = myLookUpElem element xs
--myLookUpElem 2 [(1,I 3),(2,S "abc"),(3,I 75)]
-- Just (S "abc")

-- definiti o instanta a clasei Arbitrary pt ElemIS, instante similare in curs 8

instance Arbitrary ElemIS where
    arbitrary = oneof [geni, gens]
        where 
            f = (unGen(arbitrary :: Gen Int))
            g = (unGen(arbitrary :: Gen String))
            geni = MkGen (\s i -> let x = f s i in (I x))
            gens = MkGen (\s i -> let x = g s i in (S x))

testLookUpElem :: Int -> [(Int, ElemIS)] -> Bool
testLookUpElem element list = myLookUpElem element list == lookup element list




----------------------- lab 8 ---------------------------------

type Name = String
type Quantity = Int

data Ingredient = Ing Name Quantity
    deriving Show

data Reteta = R [Ingredient]
    deriving Show


r1= R [Ing "faina" 500, Ing "oua" 4, Ing "zahar" 500]
r2= R [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 4]
r3= R [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 55]

-- sa definesc o instanta Eq pentru Ingredient

instance Eq Ingredient where
    (Ing name1 quantity1) == (Ing name2 quantity2) = map toLower name1 == map toLower name2 && quantity1 == quantity2

instance Ord Ingredient where
    (Ing n1 q1) <= (Ing n2 q2) = (map toLower n1) <= (map toLower n2) || ((map toLower n1 == map toLower n2) && q1 <= q2)


-- sa definesc o instanta Eq pentru Reteta astfel
--      sa nu conteze literele mari/ mici din numele ingredientelor
--      sa nu conteze ordinea ingredientelor
-- de exemplu, r1 == r2

eqReteta :: Reteta -> Reteta -> Bool
eqReteta (R []) (R []) = True
eqReteta (R []) _ = False
eqReteta _ (R []) = False
eqReteta (R ((Ing n1 q1):ings1)) (R ((Ing n2 q2):ings2))
    | length ings1 /= length ings2 = False
    | (Ing n1 q1) == (Ing n2 q2) = eqReteta (R ings1) (R ings2)
    | otherwise = False


instance Eq Reteta where
    (R r1) == (R r2) = eqReteta (R (sort r1)) (R (sort r2)) 


-- minutul 20-25 in lab 8 pe acolo explica pe scurt ce e cu functor, applicative, monad


-- Logica propozitionala 

type Nume = String
data Prop
    = Var Nume
    | F
    | T 
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop 
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:

-- ex1  : scrieti urmatoarele expresii de tip prop , denumindu-le p1,p2 ,p3

-- p1 = (P \/ Q) /\ (P /\ Q)
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

-- p2 = (P \/ Q) /\ (~P /\ ~Q)
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

-- p3 = (P /\ (Q \/ R)) /\ ((~P \/ ~Q) /\ (~P \/ ~R))
p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

-- ex 2 facem tipul Prop o instanta al Show, inlocuind :|:, :&:, Not cu |, &, ~ si folosind numele variabilelor 

instance Show Prop where
    show F = "F"
    show T = "T"
    show (Var p) = show p
    show (Not prop) = "(~" ++ show prop ++ ")"
    show (prop1 :&: prop2) = "(" ++ show prop1 ++ " & " ++ show prop2 ++ ")"
    show (prop1 :|: prop2) = "(" ++ show prop1 ++ " | " ++ show prop2 ++ ")"
    show (prop1 :->: prop2) = "(" ++ show prop1 ++ " -> " ++ show prop2 ++ ")"
    show (prop1 :<->: prop2) = "(" ++ show prop1 ++ " <-> " ++ show prop2 ++ ")"

-- e : V -> Bool
-- e P = True

type Env = [(Nume, Bool)]

env :: Env
env = [("P", True), ("Q", False)]

impureLookup :: Eq a => a -> [(a, b)] -> b
impureLookup x environment = fromJust . lookup x $ environment

-- sa se evalueze o propozitie
eval :: Prop -> Env -> Bool
eval (Var p) env = impureLookup p env
eval T _ = True
eval F _ = False
eval (Not p) env = not (eval p env)
eval (p1 :&: p2) env = (eval p1 env) && (eval p2 env)
eval (p1 :|: p2) env = (eval p1 env) || (eval p2 env)
eval (p1 :->: p2) env = not (eval p1 env) || (eval p2 env)
eval (p1 :<->: p2) env = (eval (p1 :->: p2) env) && (eval (p2 :->: p1) env) 

-- sa se deternime toate variabilele care sunt utilizate intr-o formula

variabile :: Prop -> [Nume]
variabile (Var p) = [p]
variabile F = []
variabile T = []
variabile (Not form) = variabile form
variabile (form1 :&: form2) = nub (variabile form1 ++ variabile form2)
variabile (form1 :|: form2) = nub (variabile form1 ++ variabile form2)
variabile (form1 :->: form2) = nub (variabile form1 ++ variabile form2)
variabile (form1 :<->: form2) = nub (variabile form1 ++ variabile form2)

-- o formula e satisfiabila daca exista un model pentru aceasta foemula 
-- daca exista un environment in care formula sa se evalueze la True

-- envs primeste o lista de stringuri, care sunt variabile propozitionale
-- si returneaza toate environmenturile

-- envs ["P", "Q"] = [[("P", False), ("Q", False)],[("P", False), ("Q", True)],[("P", True), ("Q", False)],[("P", True), ("Q", True)]]
-- ceva gen envs de P si Q = 0 0, 0 1, 1 0, 1 1

envs :: [Nume] -> [Env]
envs vars = [zip vars env | env <- sequence . take (length vars) $ repeat [False, True]]

-- o formula e satisfiabila daca are cel putin un model, adica daca exista cel putin un environment in care se evalueazala true

satisfiabila :: Prop -> Bool
satisfiabila formula = or [ eval formula env | env <- envs (variabile formula)]

-- o formula e valida ddaca negatia formulei nu este satisfiabila

p4 :: Prop
p4 = Var "P" :|: (Not (Var "P"))

valida :: Prop -> Bool
valida formula = satisfiabila (Not formula) == False

-- doua propozitii sunt echivalente daca au mereu aceeasi valoare de adevar indiferent de valorile variabilelor propozitionale

echivalenta :: Prop -> Prop -> Bool
echivalenta form1 form2 = valida (form1 :<->: form2)
















