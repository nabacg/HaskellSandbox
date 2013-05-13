 {-# LANGUAGE GADTs #-}
 
data Term a where
	Lit :: Int -> Term Int
	Succ :: Term Int -> Term Int
	IsZero :: Term Int -> Term Bool
	If :: Term Bool -> Term a -> Term a -> Term a


eval :: Term a -> a
eval (Lit i) 		= i
eval (Succ i) 		= 1 + eval i
eval (IsZero i) 	= eval i == 0	
eval (If i e1 e2)	= if eval i 
						then eval e1 
						else eval e2