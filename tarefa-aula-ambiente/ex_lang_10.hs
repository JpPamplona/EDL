type Amb = String -> Int

-------------------------------------------------------------------------------

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Var String
  deriving Show

avaliaExp :: Amb -> Exp -> Int
avaliaExp amb (Num v)     = v
avaliaExp amb (Add e1 e2) = (avaliaExp amb e1) + (avaliaExp amb e2)
avaliaExp amb (Sub e1 e2) = (avaliaExp amb e1) - (avaliaExp amb e2)
avaliaExp amb (Mul e1 e2) = (avaliaExp amb e1) * (avaliaExp amb e2)
avaliaExp amb (Var id)    = amb id

-------------------------------------------------------------------------------

data Cmd = Atr String Exp
         | Seq Cmd Cmd
         | Cnd Exp Cmd Cmd
         | Rep Exp Cmd
  deriving Show

avaliaCmd :: Amb -> Cmd -> Amb
avaliaCmd amb (Atr id exp)      = (\x -> if x==id then v
                                            else amb x)
                                where v = avaliaExp amb exp
avaliaCmd amb (Seq c1 c2)       = avaliaCmd amb' c2
                                where amb' = avaliaCmd amb c1
avaliaCmd amb (Cnd exp c1 c2)   = if (avaliaExp amb exp) /= 0 then
                                  avaliaCmd amb c1
                                else
                                  avaliaCmd amb c2

avaliaCmd amb (Rep exp c)      = if (avaliaExp amb exp) /= 0 
                                  then
                                  avaliaCmd (avaliaCmd amb c) (Rep exp c)           
                                else
                                  amb



-------------------------------------------------------------------------------

amb0 :: Amb
amb0 id = 0

p1 = Cnd (Num 0) (Atr "x" (Num 10)) (Atr "x" (Num 99))
p2 = Seq (Atr "x" (Num 10)) (Rep (Var "x") (Atr "x" (Sub (Var "x") (Num 1))))

--Soma numeros de x a y (5 a 8)
p3 =  Seq (Atr "x" (Num 5))
          (Seq  (Atr "w" (Var "x"))
                (Seq  (Atr "y" (Num 8)) 
                      (Seq  (Atr "z" (Sub (Var "y") (Var "x")))
                            (Rep (Var "z") (Seq (Atr "x" (Add (Var "x") (Add (Var "w") (Num 1))))
                                                (Seq  (Atr "w" (Add (Var "w") (Num 1))) 
                                                      (Atr "z" (Sub (Var "z") (Num 1)))))))))

--Soma dos quadrados de 1 até 10
p4 = Seq  (Atr "w" (Num 0))
          (Seq  (Atr "x" (Num 1))
                (Seq  (Atr "y" (Num 10))
                      (Seq  (Atr "z" (Var "y"))
                            (Rep (Var "z")  (Seq  (Atr "w" (Add (Var "w") (Mul (Var "x") (Var "x"))))
                                                  (Seq  (Atr "x" (Add (Var "x") (Num 1)))
                                                        (Atr "z" (Sub (Var "z") (Num 1)))))))))

main = print ((avaliaCmd amb0 p4) "w")