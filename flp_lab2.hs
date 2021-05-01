import Data.Maybe
import Data.List

type Name = String

data  Pgm  
    = Pgm [Name] Stmt
    deriving (Read, Show)

data Stmt 
    = Skip 
    | Stmt ::: Stmt 
    | If BExp Stmt Stmt 
    | While BExp Stmt 
    | Name := AExp
    deriving (Read, Show)

data AExp
    = Lit Integer 
    | AExp :+: AExp 
    | AExp :*: AExp 
    | Var Name
    deriving (Read, Show)

data BExp
    = BTrue 
    | BFalse 
    | AExp :==: AExp 
    | Not BExp
    deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:


type Env = [(Name, Integer)]

-- aEval (Var "x" :+: Lit 2) [("x", 2), ("y", 3)] -- da 4
-- bEval (Lit 1 :==: Lit 2) [("p", 0), ("n", 0)] -- False
-- bEval (Var "p" :==: Lit 2) [("p", 2), ("n", 0)] -- True
--"p" := Lit 1 ::: "n" := Lit 3 ::: While (Not (Var "n" :==: Lit 0)) ( "p" := Var "p" :*: Var "n" ::: "n" := Var "n" :+: Lit (-1) )

factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
    
pg1 = Pgm [] factStmt 



aEval :: AExp -> Env -> Integer
aEval (Lit x) env = x
aEval (a1 :+: a2) env = (aEval a1 env) + (aEval a2 env)
aEval (a1 :*: a2) env = (aEval a1 env) * (aEval a2 env)
aEval (Var x) env = fromMaybe (error "variabila nu este declarata") (lookup x env)

bEval :: BExp -> Env -> Bool
bEval (BTrue) env = True
bEval (BFalse) env = False
bEval (a1 :==: a2) env = (aEval a1 env) == (aEval a2 env)
bEval (Not b) env = not (bEval b env) 

update :: Env -> Name -> Integer -> Env
update env x i = (x, i) : filter ((x/=).fst) env
--update env x i = (x, i) : filter (\a -> fst(a)/=x) env

sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (s1 ::: s2) env = sEval s2 (sEval s1 env)   
sEval (If conditie s1 s2) env = if (bEval conditie env) then (sEval s1 env) else (sEval s2 env)
sEval (While conditie s1) env
    | (bEval conditie env) == True = sEval (While conditie s1) (sEval s1 env)
    | otherwise = sEval Skip env --env
sEval (x := a) env = update env x (aEval a env)


pEval :: Pgm -> Env
pEval (Pgm lista a) = sEval a (zip lista (repeat 0))

























   
