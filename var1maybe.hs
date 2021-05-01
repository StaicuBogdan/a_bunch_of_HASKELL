
--- Monada Maybe

{- Monada Maybe este definita in GHC.Base

instance Monad Maybe where
    return = Just
    Just a >>= k = k a
    Nothing >>= _ = Nothing

instance Applicative Maybe where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

instance Functor Maybe where
    fmap f ma = pure f <*> ma
-}


--- Limbajul si  Interpretorul

type M = Maybe

showM :: Show a => M a -> String
showM (Just a) = show a
showM Nothing = error "<eroare_maybe>"

type Name = String

data Term
    = Var Name
    | Con Integer
    | Term :+: Term
    | Term :*: Term
    | Lam Name Term
    | App Term Term
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


lookupM :: Name -> Environment -> M Value
lookupM x env = case lookup x env of
    Just v -> return v
    Nothing -> Nothing
-- lookupM x env inseamna lookup x env

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i+j)
add _ _ = Nothing

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = Nothing

mul :: Value -> Value -> M Value
mul (Num i) (Num j) = return (Num (i * j))
mul _ _ = Nothing


-- daca apelez functia test in terminal, imi afiseaza rezultatul ca string pentru ca foloseste ShowM
-- daca apelez direct interp pgm1 [], va afisa rezultatul direct pentru ca foloseeste instanta de show pt identity
test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :*: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1

pgm11:: Term
pgm11 = App
    (((Var "x") :+: (Var "x"))) ((Con 10) :+: (Con 11))

pgm2:: Term
pgm2 = App
    (Lam "x" ((Var "x") :+: (Var "y")))
    ((Con 10) :+: (Con 11))














