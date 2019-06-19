type Amb = String -> Int

data Exp = Num Int deriving Show

data Cmd = Atr String Exp
        |  Seq Cmd Cmd
        deriving Show
        
amb0 :: Amb
amb0 id = 0

avaliaExp :: Amb -> Exp -> Int
avaliaExp amb (Var id) = amb id

avaliaCmd :: Amb -> Cmd -> Amb
avaliaCmd amb (Atr id exp) = \m -> if m == id
                                    then avaliaExp amb exp
                                    else amb m
avaliaCmd amb (Seq c1 c2)  = avaliaCmd amb' c2
                             where amb' = avaliaCmd amb c1

prog = avaliaCmd amb0 (Atr "x" (Num 1))  -- \m -> if m == "x" then
                                         -- then avaliaExp amb0 (Num1)
                                         -- else amb0 m

main = print (prog)

