data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         deriving Show

avalia :: Exp -> Int
avalia (Num v) = v
avalia (Add a b) = (avalia a) + (avalia b)
avalia (Sub a b) = (avalia a) - (avalia b) 

avalia' :: Exp -> Exp
avalia' (Num v) = (Num v)
avalia' (Add (Num a) (Num b)) = Num (a + b)
avalia' (Sub (Num a) (Num b)) = Num (a - b)
avalia' (Add a b)             = (avalia' (Add (avalia' a) (avalia' b)))
avalia' (Sub a b)             = (avalia' (Sub (avalia' a) (avalia' b)))


main = print( avalia (Sub (Sub (Num 1) (Num 2)) (Sub (Num 3) (Num 4)) ))