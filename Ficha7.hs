module Ficha7 where 
 
data ExpInt = Const Int
    | Simetrico ExpInt
    | Mais ExpInt ExpInt
    | Menos ExpInt ExpInt 
    | Mult ExpInt ExpInt


e:: ExpInt 
e = Mais (Const 3) (Mult(Const 4)(Const 5)) 

--(a) Defina uma fun¸c˜ao calcula :: ExpInt -> Int que, dada uma destas express˜oes calcula o seu valor.

calcula :: ExpInt -> Int
calcula (Const x )= x
calcula (Simetrico x) = - (calcula x) 
calcula (Mais x y) = calcula x + calcula y 
calcula (Menos x y) = calcula x - calcula y 
calcula (Mult x y) = calcula x * calcula y 


--(b) Defina uma fun¸c˜ao infixa :: ExpInt -> String de forma a queinfixa (Mais (Const 3) (Menos (Const 2) (Const 5))) dˆe como resultado"(3 + (2 - 5))".

infixa :: ExpInt -> String
infixa (Const x) = show x 
infixa (Simetrico x) = "(- " ++ infixa x ++ ")"
infixa (Mais x y) = "(" ++ infixa x ++ " + " ++ infixa y ++ ")"
infixa (Menos x y) = "(" ++ infixa x ++ " - " ++ infixa y ++ ")"
infixa (Mult x y ) = "(" ++ infixa x ++ " * " ++ infixa y ++ ")"


--(c) Defina uma outra fun¸c˜ao de convers˜ao para strings posfixa :: ExpInt -> String de forma a que quando aplicada `a express˜ao acima dˆe como resultado "3 2 5 - +".

posfixa :: ExpInt -> String
posfixa (Const x) = show x 
posfixa (Simetrico x) = (posfixa x) ++ "-"
posfixa (Mais x y) = posfixa x ++ posfixa y ++ "+"
posfixa (Menos x y) = posfixa x ++ posfixa y ++ "-"
posfixa (Mult x y) = posfixa x ++ posfixa y ++ "*"

-- 2)

data RTree a = R a [RTree a]
             deriving Show 

--(a) soma :: Num a => RTree a -> a que soma os elementos da ´arvore


arvore :: RTree Int
arvore = R 7 [R 3 [R 1 []
                  ,R 2 []]
              ,R 10 []
              ,R 4 [R 1 [R 5 []
                      ,R 8  []
                      ,R 12 []]
                 ]
            ]

soma :: Num a => RTree a -> a
soma (R a []) = a
soma(R a xs) = a + sum( map soma xs) 

--foldr
soma' :: Num a => RTree a -> a
soma' (R i l) = i + foldr (\t r-> soma t +r) 0 l

--(c) prune :: Int -> RTree a -> RTree a que remove de uma ´arvore todos os elementos a partir de uma determinada profundidade.

prune :: Int -> RTree a -> RTree a
prune 1 (R x _) = (R x [])
prune n (R x l) =  R x (map(prune (n-1))l)

prune' :: RTree a -> Int -> RTree a 
prune' (R x _) 1 = (R x [])
prune' (R x l) n = R x  (map(\t -> (prune' t (n-1)))l)

prune'' :: RTree a -> Int -> RTree a 
prune'' (R x _) 1 = (R x [])
prune'' (R x l) n = R x  (map(((flip prune')(n-1)))l)

--(d) mirror :: RTree a -> RTree a que gera a ´arvore sim´etrica.
mirror :: RTree a -> RTree a 
mirror (R x []) = (R x [])
mirror (R x l) =  R x (reverse (map mirror l))

-- 3)

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a 
              | Fork (LTree a) (LTree a)
             deriving Show 

arvore6 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))

--(a) ltSum :: Num a => LTree a -> a que soma as folhas de uma ´arvore

ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n 
ltSum (Fork n m) = ltSum n + ltSum m 


-- (b) listaLT :: LTree a -> [a] que lista as folhas de uma ´arvore (da esquerda paraa direita).

listaLT :: LTree a -> [a]
listaLT (Tip i) = [i]
listaLT (Fork e d) = listaLT e ++ listaLT d

-- (c) ltHeight :: LTree a -> Int que calcula a altura de uma ´arvore.

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)

--4)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)


-- (a) Defina a fun¸c˜ao splitFTree :: FTree a b -> (BTree a, LTree b) que separauma ´arvore com informa¸c˜ao nos nodos e nas folhas em duas ´arvores de tipos diferentes.

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty, Tip n)
splitFTree (No a l r) = (Node a lb rb, Fork ll rl)
    where (lb, ll) = splitFTree l
          (rb, rl) = splitFTree r 


--(b) Defina ainda a fun¸c˜ao joinTrees :: BTree a -> LTree b -> Maybe (FTree a b) que sempre que as ´arvores sejam compat´ıveis as junta numa s´o. 
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) =
    case (joinTrees l a, joinTrees r b) of (Just x, Just y) -> Just (No e x y)
                                           _ -> Nothing
joinTrees _ _ = Nothing