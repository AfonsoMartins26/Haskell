module TestePf.Fichapf7 where

--1
data ExpInt = Const Int
          | Simetrico ExpInt
          | Mais ExpInt ExpInt
          | Menos ExpInt ExpInt
          | Mult ExpInt ExpInt

{-(a) Defina uma fun¸c˜ao calcula :: ExpInt -> Int que, dada uma destas express˜oes
calcula o seu valor-}

calcula :: ExpInt -> Int
calcula (Const n) = n 
calcula (Simetrico exp) = (- calcula exp)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b )= calcula a - calcula b 
calcula (Mult a b )= calcula a * calcula b 

{-(b) Defina uma fun¸c˜ao infixa :: ExpInt -> String de forma a que
infixa (Mais (Const 3) (Menos (Const 2) (Const 5))) dˆe como resultado
"(3 + (2 - 5))".-}

infixa :: ExpInt -> String 
infixa (Const n) = show n 
infixa (Simetrico exp) = "(-(" ++ infixa exp ++ "))"
infixa (Mais a b) = "(" ++ infixa a ++ " + " ++ infixa b  ++ ")"
infixa (Menos a b) = "(" ++ infixa a ++ " - " ++ infixa b ++ ")"
infixa (Mult a b) = "(" ++ infixa a ++ " * " ++ infixa b ++ ")"

{-(c) Defina uma outra fun¸c˜ao de convers˜ao para strings posfixa :: ExpInt -> String
de forma a que quando aplicada `a express˜ao acima dˆe como resultado "3 2 5 -
+".-}

posfixa :: ExpInt -> String
posfixa (Const n) = show n 
posfixa (Simetrico exp) = posfixa exp ++ " (-) "
posfixa (Mais a b) =posfixa a ++" " ++ posfixa b ++" + "
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " - "
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " * "


--2
data RTree a = R a [RTree a]

arvore = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]

--(a) soma :: Num a => RTree a -> a que soma os elementos da ´arvore.
soma :: Num a => RTree a -> a
soma (R e []) = e 
soma (R e es) = e + sum (map soma es)

--(b) altura :: RTree a -> Int que calcula a altura da ´arvore.

altura :: RTree a -> Int
altura (R e []) = 1
altura (R e es) = 1 + maximum (map altura es )

{-(c) prune :: Int -> RTree a -> RTree a que remove 
de uma ´arvore todos os elementos a partir de uma determinada profundidade.-}

prune :: Int -> RTree a -> RTree a 
prune 0 (R e es) = R e []
prune n (R e es) = R e (map(prune (n-1) )es) 

--(d) mirror :: RTree a -> RTree a que gera a ´arvore sim´etrica

mirror :: RTree a -> RTree a
mirror (R e es ) = R e ( map mirror (reverse es))

--(e) postorder :: RTree a -> [a] que corresponde `a travessia postorder da ´arvore.

postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e es) =  concatMap postorder es ++ [e] -- aplica a funcao postorder na lista es e  depois
                                                    -- torna uma lista de listas e torna a numa lista unica 
--3
data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

arvore2 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))

--(a) ltSum :: Num a => LTree a -> a que soma as folhas de uma ´arvore
ltSum :: Num a => LTree a -> a 
ltSum (Tip n) = n 
ltSum (Fork a b) = ltSum a + ltSum b 

{-(b) listaLT :: LTree a -> [a] que lista as folhas de uma ´arvore (da esquerda para
a direita).-}

listaLT :: LTree a -> [a]
listaLT (Tip n) = [n]
listaLT (Fork a b) = listaLT a ++ listaLT b 

-- (c) ltHeight :: LTree a -> Int que calcula a altura de uma ´arvore.

ltHeight :: LTree a -> Int 
ltHeight (Tip _) = 0 
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b) 

-- 4 
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) 

{-(a) Defina a fun¸c˜ao splitFTree :: FTree a b -> (BTree a, LTree b) que separa
uma ´arvore com informa¸c˜ao nos nodos e nas folhas em duas ´arvores de tipos
diferentes.-}

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree(Leaf e) = (Empty,Tip e)
splitFTree (No e l r) = (Node e lb rb,Fork ll rl)
                      where (lb,ll)= splitFTree l 
                            (rb,rl) = splitFTree r 

{-(b) Defina ainda a fun¸c˜ao joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
que sempre que as ´arvores sejam compat´ıveis as junta numa s´o.-}

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip e) = Just (Leaf e) 
joinTrees (Node e l r) (Fork a b) =
    case (joinTrees l a, joinTrees r b) of (Just x, Just y) -> Just (No e x y)
                                           _ -> Nothing -- se as tentativas de fazer uma combinação falharem 
                                                        -- a funcao vai retornar Nothing 
joinTrees _ _ = Nothing
