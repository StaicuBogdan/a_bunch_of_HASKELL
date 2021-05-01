
--- Monada StringWriter

newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }

instance Show a => Show (StringWriter a) where
    show ma = "Output: " ++ w ++ " Value: " ++  show b
               where (b, w) = runStringWriter ma

instance Monad StringWriter where
    return a = StringWriter (a, "")
    ma >>= k = let (a, log1) = runStringWriter ma
                   (b, log2) = runStringWriter (k a)
                   in StringWriter (b, log1 ++ log2)


instance Applicative StringWriter where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

instance Functor StringWriter where
    fmap f ma = pure f <*> ma

tell :: String -> StringWriter ()
tell log = StringWriter ((), log) -- produce mesajul

--- Limbajul si  Interpretorul
-- constructorul unei monade este unar, deci ii dam either string ca sa mai ia doar un parametru


type Environment = [(Name, Value)] -- [ ("p", (Num 1)) ]
type M a = StringWriter a

showM :: Show a => M a -> String
showM ma = "Output: " ++ w ++ " Value: " ++ show a
    where (a, w) = runStringWriter ma

type Name = String

data Term
    = Var Name
    | Con Integer
    | Term :+: Term
    | Term :*: Term
    | Lam Name Term
    | App Term Term
    | Out Term
    deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"



interp :: Term -> Environment -> M Value
interp (Con i) env = return (Num i) -- Just (Num i)
interp (Var x) env = lookupM x env 
interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2
interp (Lam x e) env = return $ Fun $ \v -> interp e ((x,v):env)
interp (App t1 t2) env = do
    f <- interp t1 env
    v <- interp t2 env
    apply f v
interp (t1 :*: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    mul v1 v2
interp (Out t) env = do
    v <- interp t env
    tell (show v ++ "; ")
    return v


lookupM :: Name -> Environment -> M Value
lookupM x env = case lookup x env of
    Just v -> return v
    Nothing -> return Wrong

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i+j)
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = return Wrong

mul :: Value -> Value -> M Value
mul (Num i) (Num j) = return (Num (i * j))
mul _ _ = return Wrong



-- daca apelez functia test in terminal, imi afiseaza rezultatul ca string pentru ca foloseste ShowM
-- daca apelez direct interp pgm1 [], va afisa rezultatul direct pentru ca foloseeste instanta de show pt identity
test :: Term -> String
test t = showM $ interp t []

pgm :: Term
pgm = App
  (Lam "y" (App (App (Lam "f"(Lam "y" (App (Var "f") (Var "y")))) (Lam "x" (Var "x" :+: Var "y")))(Con 3)))(Con 4)

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :*: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1

pgm11:: Term
pgm11 = App
    (((Var "x") :+: (Var "x"))) ((Con 10) :+: (Con 11))
-- eroare : nu s-au gasit variabilele


pgm2:: Term
pgm2 = App
    (Lam "x" ((Var "x") :+: (Var "y")))
    ((Con 10) :+: (Con 11))
-- eroare : nu s-au gasit variabilele


pgm3 = App
    (Con 10) (Con 11)
-- eroare : nu e functie


-- pgm41 :: Term
-- pgm41 = App
    -- (Lam "x" ((Var "x") :+: (Var "x"))) (Amb ((Con 10) :+: (Con 11)) (Con 2))
-- -- face o lista de valori pe care sa aplice functia aia lambda

-- pgm42 :: Term
-- pgm42 = App
    -- (Lam "x" ((Var "x") :+: (Var "x"))) (Amb (Con 5) (Amb ((Con 10) :+: (Con 11)) (Con 2)))

pgmW :: Term
pgmW = App 
        (Lam "x" ((Var "x") :+: (Var "x")))
        ((Out (Con 10)) :+: (Out (Con 11)))

pgm5 = Out( Out (Out (Con 41) :+: Out (Con 5)))








