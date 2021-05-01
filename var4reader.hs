
--- Monada Reader
type Environment = [(Name, Value)] -- [ ("p", (Num 1)) ]
newtype EnvReader a = Reader { runEnvReader :: Environment -> a }

instance (Show a) => Show (EnvReader a) where
     show (Reader f) = show $ f []

instance Monad (EnvReader) where
    return a = Reader (\_ -> a)
    ma >>= k = Reader f
               where f env = let va = runEnvReader ma env
                             in runEnvReader (k va) env

instance Applicative (EnvReader) where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

instance Functor (EnvReader) where
    fmap f ma = pure f <*> ma

ask :: EnvReader Environment -- obt ine memoria
ask = Reader id -- Reader ( \ env -> env )

local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma = Reader $ (\r -> (runEnvReader ma) (f r))

--- Limbajul si  Interpretorul
-- constructorul unei monade este unar, deci ii dam either string ca sa mai ia doar un parametru


type M a = EnvReader a

showM :: Show a => M a -> String
showM x = show (runEnvReader x [])

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



interp :: Term -> M Value
interp (Con i) = return (Num i) 
interp (Var x) = lookupM x
interp (t1 :+: t2) = do
    v1 <- interp t1
    v2 <- interp t2
    add v1 v2
interp (Lam x e) = do
    env <- ask
    return $ Fun $ \v -> local (const ((x,v):env)) (interp e)
interp (App t1 t2) = do
    f <- interp t1
    v <- interp t2
    apply f v
interp (t1 :*: t2) = do
    v1 <- interp t1
    v2 <- interp t2
    mul v1 v2


lookupM :: Name -> M Value
lookupM x = do
    env <- ask
    case lookup x env of
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
test t = showM $ interp t

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












