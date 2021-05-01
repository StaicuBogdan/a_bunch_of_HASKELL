

---------------------- lab 9 ---------------------------------
data Expr
    = Const Int
    | Expr :+: Expr
    | Expr :*: Expr
    deriving Eq

data Operation = Add | Mult
    deriving (Eq, Show)

data Tree
    = Lf Int
    | Node Operation Tree Tree
    deriving (Eq, Show)

{-      (*)
        / \ 
      (+)  5
      /  \
     2    3
-}

arb1 = Node Mult (Node Add (Lf 2) (Lf 3)) (Lf 5)

-- ex 1  sa se instantieze clasa show pentru Expr

instance Show Expr where
    show (Const x) = show x
    show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

-- ex 2: sa se scrie o functie 
evalExp :: Expr -> Int
evalExp (Const a )= a
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

exp1 = ((Const 2 :+: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+:(Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

-- ex 3 : sa se scrie o functie evalArb care evalueaza arbvori de expresii
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add l r) = evalArb l + evalArb r
evalArb (Node Mult l r) = evalArb l * evalArb r

-- ex 4 : sa se scrie o functie care transforma o expresie intru-un arbore 
expToArb :: Expr -> Tree
expToArb (Const a) = Lf a
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)

-- ex 5 : sa se instantieze clasa MySmallCheck pentru tipul de date Expr
-- lista de valori continand cateva expresii definite de voi

class MySmallCheck a where
    smallValues :: [a]
    smallCheck :: (a -> Bool) -> Bool
    smallCheck prop = and [prop x | x <- smallValues]

instance MySmallCheck Expr where
    smallValues = [exp1, exp2, exp3, exp4]

-- sa se verifice evaluarea unei expresii e egala cu evaluarea arborelui asociat expresiei
checkExpr :: Expr -> Bool
checkExpr e = evalExp e == evalArb (expToArb e) 

-- deci smallcheck evalueaza functia checkExpr ( care ia doar o expresie si o verifica) pe setul de expresii
-- din instanta definita , adica exp1, exp2, exp3, exp4


-- definim clasa Collection
type Key = Int
type Value = String
-- Key si Value sunt doar sinonime pentru Int si String, dar stiam deja

class Collection c where
    cempty :: c
    csingleton :: Key -> Value -> c
    cinsert :: Key -> Value -> c -> c
    cdelete :: Key -> c -> c
    clookup :: Key -> c -> Maybe Value
    cToList :: c -> [(Key, Value)]

    ckeys :: c -> [Key]
    ckeys c = [fst p | p <- cToList c]
    cvalues :: c -> [Value]
    cvalues c = [snd p | p <- cToList c]
    cFromList :: [(Key, Value)] -> c
    cFromList [] = cempty
    cFromList ((k, v):kvs) = cinsert k v (cFromList kvs)

-- sa se adauge definitii implicite pentru ckeys, cvalues, cFromList pe baza celorlalte functii din clasa Collection


newtype PairList = PairList { getPairList :: [(Key, Value)]}

-- data PairList = PairList [(Key, Value)]

-- o insanta a clasei Show
-- o instanta a clasei Collection

instance Show PairList where
    show (PairList pairList) = "PairList" ++ show pairList

instance Collection PairList where
    cempty = PairList []
    csingleton k v = PairList [(k, v)]
    cinsert k v (PairList list) = if elem k (map fst list) then PairList list else PairList ((k, v) : list)
    cdelete k (PairList list) = PairList [(k1, v1) | (k1, v1) <- list, k1 /= k]
    clookup k (PairList list) = lookup k list
    cToList = getPairList


--data SearchTree
--    = Empty
--    | Node SearchTree Key (Maybe Value) SearchTree
--    deriving Show
-- in stanga sunt elementele cu cheie mai mica
-- in dreapta sunt elementele cu cheie mai mare
-- sa se instantieze clasa Collection pentru tipul de date SearchTree

--instance Collection SearchTree where
--    cempty = Empty
--    csingleton k v = Nod Empty k (Just v) Empty
--    cinsert k v Empty = csingleton k v
--    cinsert k v (Nod l k1 (Just v1) r)
--        | k == k1 = Nod l k1 (Just v1) r
--        | k < k1 = Nod (cinsert k v l) k1 (Just v1) r
--        | k > k1 = Nod l k1 (Just v1) (cinsert k v r)

-- ramane sa completez eu candva cica


-- FUNCTORI

data Arbore a
    = Frunza a
    | Nod a (Arbore a) (Arbore a)
    deriving (Eq, Show)
--vreau sa fac o instanta a clasei Functor pt tipul de date arbore
-- primind o functie (a-> b), un Arbore a => Arbore b

instance Functor Arbore where
    fmap f (Frunza x) = Frunza (f x)
    fmap f (Nod x l r) = Nod (f x) (fmap f l) (fmap f r)

-- pot utiliza <$>

arbore = Nod 2 (Nod 3 (Frunza 2) (Frunza 3)) (Frunza 5)

-- video lab 10 in ultimele 13 minute explica functori foarte misto , dar pe scurt :)
















