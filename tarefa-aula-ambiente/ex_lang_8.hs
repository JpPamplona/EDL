type Amb = String -> Int 

data Exp = Num Int
            | Add Exp Exp
            | Sub Exp Exp
            | Var String 
            deriving Show  

data Cmd = Atr String Exp
            | Seq Cmd Cmd 
            deriving Show

avaliaExp :: Amb -> Exp -> Int
avaliaExp amb (Num v)           = v
avaliaExp amb (Add exp1 exp2)   = (avaliaExp amb exp1) + (avaliaExp amb exp2)
avaliaExp amb (Sub exp1 exp2)   = (avaliaExp amb exp1) - (avaliaExp amb exp2)
avaliaExp amb (Var id)          = amb id

avaliaCmd :: Amb -> Cmd -> Amb
avaliaCmd amb (Atr id exp) = \m -> if m == id
                                    then avaliaExp amb exp
                                    else amb m
avaliaCmd amb (Seq c1 c2)  = avaliaCmd (avaliaCmd amb c1) c2

avaliaProg :: Cmd -> Int
avaliaProg (Atr id exp) = \m -> if m == id
                                then avaliaExp amb exp
                                else amb m




amb0 :: Amb
amb0 id = 0          

prog :: Cmd
prog = Seq  (Atr "x" (Num 10)) 
            (Seq    (Atr "y" (Var "x")) 
                    (Atr "x" (Num 20)))

main = print ((avaliaCmd amb0 prog) "x")