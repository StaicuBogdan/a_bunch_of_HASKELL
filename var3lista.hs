
--- Monada Listelor

--- Limbajul si  Interpretorul

type M a = [a]

showM :: Show a => M a -> String
showM = show

type Name = String

data Term
    = Var Name
    | Con Integer
    | Term :+: Term
    | Term :*: Term
    | Lam Name Term
    | App Term Term
    | Amb Term Term
    | Fail
    deriving (Show)

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)


data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)] -- [ ("p", (Num 1)) ]

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
interp Fail _ = []
interp (Amb t1 t2) env = (interp t1 env) ++ (interp t2 env) 

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
-- daca apelez direct interp pgm1 [], va afisa rezultatul direct pentru ca foloseste instanta de show pt identity
test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :*: (Var "x")))
          ((Con 10) :+:  (Con 11))

pgm11:: Term
pgm11 = App
    (((Var "x") :+: (Var "x"))) ((Con 10) :+: (Con 11))

pgm2:: Term
pgm2 = App
    (Lam "x" ((Var "x") :+: (Var "y")))
    ((Con 10) :+: (Con 11))

pgm3 :: Term
pgm3 = App
    (Con 10) (Con 11)
-- eroare : nu e functie

pgm41 :: Term
pgm41 = App
    (Lam "x" ((Var "x") :+: (Var "x"))) (Amb ((Con 10) :+: (Con 11)) (Con 2))
-- face o lista de valori pe care sa aplice functia aia lambda

pgm42 :: Term
pgm42 = App
    (Lam "x" ((Var "x") :+: (Var "x"))) (Amb (Con 5) (Amb ((Con 10) :+: (Con 11)) (Con 2)))








