data Cmd = Atr String Exp
            | Seq Cmd Cmd 
            deriving Show

data Exp = Num Int
            | Add Exp Exp
            | Sub Exp Exp
            | Var String 
            deriving Show   

            
c0 :: Cmd
c0 = Atr "x" (Num 1)

c1 :: Cmd
c1 = Atr "x" (Sub (Add (Num 1) (Var "x")) (Num 20))

c2 :: Cmd
c2 = Seq (Atr "z" (Add (Var "x") (Var "y")) (Seq (Atr "x" (Num 1)) (Atr "y" (Num 2)))

main = print c2