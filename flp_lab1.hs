import Data.Maybe
import Data.List
----------------------lab 1 ----------------------------

data Prog 
    = On Instr

data Instr
    = Off 
    | Expr :> Instr

data Expr 
    = Mem 
    | V Int 
    | Expr :+ Expr

type Env = Int -- valoarea celulei de memorie
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int


p1 = On ((V 3) :> ((Mem :+ (V 5)):> Off))
p2 = On ((V 3) :> ((V 7) :> ((V 6) :> ((Mem :+ (V 5)) :> Off))))

prog :: Prog -> DomProg
prog (On s) = stmt s 0

stmt :: Instr -> DomInstr
stmt (e :> s) m = (expr e m):(stmt s (expr e m))
stmt Off _ = []

expr :: Expr -> DomExpr
expr Mem m = m
expr (V x) m = x
expr (e1 :+ e2) m = (expr e1 m) + (expr e2 m)

type Name = String
data Hask 
    = HTrue
    | HFalse
    | HLit Int
    | HIf Hask Hask Hask
    | Hask :==: Hask
    | Hask :+: Hask
    | HVar Name
    | HLam Name Hask
    | Hask :$: Hask
    deriving (Read, Show)
infix 4 :==:
infixl 6 :+:
infixl 9 :$:


data Value 
    = VBool Bool
    | VInt Int
    | VFun (Value -> Value)
    | VError -- pentru reprezentarea erorilor

type HEnv = [(Name, Value)]
type DomHask = HEnv -> Value


instance Show Value where
    show (VBool x) = show x
    show (VInt x) = show x
    show (VFun f) = "functie"
    show (VError) = "eroare"

instance Eq Value where
    (VBool x1) == (VBool x2) = x1 == x2
    (VInt x1) == (VInt x2) = x1 == x2
    _ == _ = error "nu se poate verifica egalitatea"

hEval :: Hask -> HEnv -> Value
hEval HTrue env = VBool True
hEval HFalse env = VBool False
hEval (HLit i) env = VInt i
hEval (HIf conditie atunci altfel) env = hif (hEval conditie env) (hEval atunci env) (hEval altfel env)
                                         where hif (VBool x) a b = if x then a else b
                                               hif _ _ _ = VError
hEval (h1 :==: h2) env = VBool (hEval h1 env == hEval h2 env)
hEval (d :+: e) env = hadd (hEval d env) (hEval e env)
                      where hadd (VInt i) (VInt j) = VInt (i + j)
                            hadd _ _ = VError
hEval (HVar x) env = fromMaybe VError (lookup x env)
hEval (HLam x e) env = VFun (\v -> hEval e ((x,v):env))
hEval (d :$: e) env = happ (hEval d env) (hEval e env)
                      where happ (VFun f) v = f v
                            happ _ _ = VError

run :: Hask -> String
run pg = show (hEval pg [])

h0 = (HLam "x" (HLam "y" ((HVar "x") :+: (HVar "y")))) :$: (HLit 3) :$: (HLit 4)
h1 = hEval (HIf HTrue (HVar "x" :+: HVar "y") (HVar "x")) [("x", VInt 2), ("y", VInt 30)] -- 32
h2 = hEval (HIf HFalse (HVar "x" :+: HVar "y") (HVar "x")) [("x", VInt 2), ("y", VInt 30)] -- 32

h3 = hEval (HVar "x") [("x", VInt 2), ("y", VInt 30)]
h4 = hEval (HIf (HFalse :==: HVar "x") (HVar "x" :+: HVar "y") (HVar "x")) [("x", VInt 2), ("y", VInt 30)]










