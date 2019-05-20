data Arvore = Folha | Galho Arvore Arvore
      deriving Show

a1 = Galho (Galho Folha Folha) (Galho Folha Folha)
a2 = Galho (Galho (Galho Folha Folha) Folha) Folha
a3 = Galho Folha (Galho (Galho Folha (Galho Folha Folha)) (Galho Folha Folha))

folhas :: Arvore -> Int
folhas Folha        = 1
folhas (Galho d e)  = (folhas e) + (folhas d)

altura :: Arvore -> Int
altura Folha          = 0
altura (Galho d e)    = if(altura d > altura e)
                        then (1 + altura d)
                        else (1 + altura e)

espelho :: Arvore -> Arvore
espelho Folha         = Folha
espelho (Galho d e)   = Galho (espelho e) (espelho d)

main = print (espelho a3)