-- POINARITA ANDREEA DIANA, GRUPA 243

{-
Gasiti mai jos  un minilimbaj. Interpretarea este partial definita.
Un program este o expresie de tip `Pgm`iar rezultatul executiei este ultima stare a memoriei. 
Executia unui program se face apeland `pEval`.
-}

{-
import Data.Maybe
import Data.List

type Name = String

data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt| Name := AExp | While BExp Stmt
        deriving (Read, Show)

data AExp = Lit Integer | AExp :+: AExp | AExp :*: AExp | Var Name
        deriving (Read, Show)

data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:


type Env = [(Name, Integer)]


aEval :: AExp -> Env -> Integer
aEval (Lit i) _ = i
aEval (exp1 :+: exp2) env =  (aEval exp1 env) + (aEval exp2 env)
aEval (exp1 :*: exp2) env =  (aEval exp1 env) * (aEval exp2 env)
aEval (Var x) env = fromMaybe (error "Variabila nu este declarata") (lookup x env)



bEval :: BExp -> Env -> Bool
bEval BTrue _ = True
bEval BFalse _ = False
bEval (exp1 :==: exp2) env = (aEval exp1 env) == (aEval exp2 env)
bEval (Not exp) env = not (bEval exp env)



sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (st1 ::: st2) env = sEval st2 (sEval st1 env)
sEval (If b st1 st2) env =  if (bEval b env) then (sEval st1 env) else (sEval st2 env) 
sEval (x := exp) env = (x,aEval exp env):[e | e<-env, (fst e)/= x]
sEval (While bexp st) env = case (bEval bexp env) of
        True -> sEval (While bexp st) (sEval st env)
        False -> env



pEval :: Pgm -> Env
pEval (Pgm lvar st) = sEval st [(x,0)| x<-lvar]

 
factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
    
test1 = Pgm [] factStmt 
-}

{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10pct) Adaugati instructiunea `While BExp Stmt` si interpretarea ei.
3) (20pct) Definiti interpretarea limbajului astfel incat programele sa se execute dintr-o stare 
initiala data iar  `pEval`  sa afiseze starea initiala si starea finala.    

Definiti teste pentru verificarea solutiilor si indicati raspunsurile primite. 

-}

-- C)
import Data.Maybe
import Data.List

type Name = String

data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt| Name := AExp | While BExp Stmt
        deriving (Read, Show)

data AExp = Lit Integer | AExp :+: AExp | AExp :*: AExp | Var Name
        deriving (Read, Show)

data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:

type Env = [(Name, Integer)]

newtype EnvReader a = EnvReader {runEnvReader :: Env -> a}


instance Monad EnvReader where
    return x = EnvReader (\_ -> x)
    ma >>= k = EnvReader f
        where f env = let a = runEnvReader ma env    
                      in runEnvReader (k a) env      

instance Applicative EnvReader where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor EnvReader where
  fmap f ma = pure f <*> ma


ask :: EnvReader Env
ask = EnvReader id


local :: (Env -> Env) -> EnvReader a -> EnvReader a
local f ma = EnvReader $ (\env -> (runEnvReader ma )(f env))




aEval :: AExp -> EnvReader Integer
aEval (Lit i) = return i
aEval (exp1 :+: exp2) =  do
                           val1 <- aEval exp1 
                           val2 <- aEval exp2
                           return (val1+val2)
aEval (exp1 :*: exp2) =  do
                           val1 <- aEval exp1 
                           val2 <- aEval exp2
                           return (val1*val2)
aEval (Var x) = lookupM x


lookupM :: Name -> EnvReader Integer 
lookupM var = do
                env <- ask
                case lookup var env of
                    Just val -> return val
                    Nothing -> error "Variabila nu este declarata"




bEval :: BExp -> EnvReader Bool
bEval BTrue = return True
bEval BFalse = return False
bEval (exp1 :==: exp2)  = do
                              val1 <- aEval exp1 
                              val2 <- aEval exp2 
                              return (val1==val2)
bEval (Not exp) = do
                         val <- bEval exp
                         return (not val)



sEval :: Stmt -> EnvReader Env
sEval Skip = do
              env <-ask
              return env
sEval (If b st1 st2) = do
        cond <- bEval b 
        if cond then (sEval st1) else (sEval st2) 

sEval (st1 ::: st2) =  do
        valst <- sEval st1 -- env
        local (\_ -> valst) (sEval st2)

sEval (x := exp) = do
        valexp <- aEval exp -- integer
        env <- ask
        return $ ((x,valexp):[e | e<-env, (fst e)/= x])


sEval (While bexp st) = do 
                   cond <- bEval bexp
                   env <-ask
                   if cond then 
                           do
                                newenv <- sEval st 
                                local (const $ newenv) (sEval (While bexp st))
                   else
                           do
                              return env

showStari::Stmt -> EnvReader String
showStari st = do
        env <-ask
        finalenv <- sEval st
        return ("Initial: " ++ show env ++ " Final: " ++ show finalenv)
        

pEval :: Pgm -> String
pEval (Pgm lvar st) = runEnvReader (showStari st) [(x,0)| x<-lvar]


factStmt :: Stmt
factStmt =
  "p" := Lit 2 ::: "n" := Lit 3 ::: 
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1) :::
      "r" := Var "r" :+: Lit (1)
    )
    
--test1 :: Pgm  
test1 = Pgm ["r"] factStmt 
