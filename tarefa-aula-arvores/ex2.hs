data Arvore = Folha | Galho Int Arvore Arvore
    deriving Show

a1 = Galho 10 (Galho 10 Folha Folha) (Galho 10 Folha Folha)
a2 = Galho 10 (Galho 10 (Galho 20 Folha Folha) Folha) Folha
a3 = Galho 10 Folha (Galho 10 (Galho 20 Folha Folha) (Galho 10 Folha Folha))

folhas :: Arvore -> Int
folhas Folha          = 1
folhas (Galho _ d e)  = (folhas e) + (folhas d)

altura :: Arvore -> Int
altura Folha            = 0
altura (Galho _ d e)    = if(altura d > altura e)
                        then (1 + altura d)
                        else (1 + altura e)

espelho :: Arvore -> Arvore
espelho Folha           = Folha
espelho (Galho v d e)   = Galho v (espelho e) (espelho d)    

soma :: Arvore -> Int
soma Folha           = 0
soma (Galho v e d)   = v + (soma e) + (soma d)

dobra :: Arvore -> Arvore
dobra Folha         = Folha
dobra (Galho v e d) = Galho (2*v) (dobra e) (dobra d)

possui :: Int -> Arvore -> Bool
possui _ Folha          = False
possui v1 (Galho v e d) = (v == v1) || (possui v1 e) || (possui v1 d)

main = print (possui 100 a1)