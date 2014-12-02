{-
  Напишите программу, решающую следующую задачу методом полного перебора:

     «Крестьянину нужно перевезти через реку волка, козу и капусту. Но лодка такова,
     что в ней может поместиться только крестьянин,  а с ним или один волк, или одна
     коза, или одна капуста.  Но если оставить волка с козой,  то волк съест козу, а
     если оставить  козу с капустой,  то коза  съест капусту.  Как перевёз свой груз
     крестьянин?»

  В качестве идеи для реализации используйте решение задачи о калотанской семье
  (kalotans-puzzle.hs). 
-}

type Var = String
type Value = String
data Predicate =
	Is Var Value
	| Equal Var Var
	| And Predicate Predicate
	| Or Predicate Predicate
	| Not Predicate
		deriving (Eq, Show)
type Variables = [(Var, Value)]


isNot :: Var -> Value -> Predicate
isNot var value = Not (Is var value)

implies :: Predicate -> Predicate -> Predicate
implies a b = Not (a `And` (Not b))

orElse :: Predicate -> Predicate -> Predicate
orElse a b = (a `And` (Not b)) `Or` ((Not a) `And` b)


check :: Predicate -> Variables -> Maybe Bool
check (Is var value) vars = liftM (==value) (lookup var vars)
check (Equal v1 v2) vars = liftM2 (==) (lookup v1 vars) (lookup v2 vars)
check (And p1 p2) vars = liftM2 (&&) (check p1 vars) (check p2 vars)
check (Or p1 p2) vars = liftM2 (||) (check p1 vars) (check p2 vars)
check (Not p) vars = liftM not (check p vars)


main = undefined
