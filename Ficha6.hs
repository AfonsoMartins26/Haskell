{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Pftrees where


data BTree a = Empty
             | Node a (BTree a) (BTree a)
    deriving (Show)

arv1 = Node 3 (Node 5 Empty Empty)
              (Node 7(Node 6 Empty Empty)
                     (Node 0 Empty Empty))

arv2 = Node 10 arv1 (Node 1 arv1 Empty)

altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1+ max (altura e) (altura d)

procura :: Eq a => a -> BTree a -> Bool
procura x Empty = False
procura x (Node y e d) = x==y || (procura x e) || (procura x d)

elemAltura :: Eq a=> a -> BTree a -> [Int]
elemAltura x arv = elAltura x arv 1

elAltura :: Eq a => a -> BTree a -> Int -> [Int]
elAltura x Empty n = []
elAltura x (Node y e d) n 
            | x==y = n:(elAltura x e (n+1))++ (elAltura x d(n+1))
            | otherwise = (elAltura x e (n+1))++(elAltura x d (n+1))
            
--travessias 
infixa:: BTree a -> [a]
infixa Empty = []--árvore esquerda, depois a raiz e a seguir a árvore direita
infixa (Node x e d) = (infixa e)++[x]++(infixa d)

prefixa:: BTree a -> [a]--raiz depois a árvore esquerda e a seguir a árvore direita
prefixa Empty = []
prefixa (Node x e d) = x:(prefixa e)++(prefixa d)

posfixa:: BTree a -> [a]
posfixa Empty = []--árvore esquerda, depois árvore direita, e a seguir a raiz.
posfixa (Node x e d) =(posfixa e)++(posfixa d)++[x]


testeprocura :: Ord a => BTree a -> Bool 
testeprocura Empty = True 
testeprocura (Node x e d) = testeprocura e && testeprocura d 
                          && all (x>) (posfixa e) && all (>x) (posfixa d)

testProc :: Ord a => BTree a -> Bool 
testProc Empty = True 
testProc (Node x e@( Node x1 e1 d1)d@(Node x2 e2 d2))=    -- e@ é como se o  proximo argumento fosse chamado de e 
       x>x1 && x<x2 && testProc e && testProc d 


toList :: BTree a -> ([a],Int)
toList Empty = ([],0)
toList (Node x e d) = (  l1 ++[x]++ l2,1 + n1 + n2 )
    where (l1,n1) = toList e 
          (l2,n2) = toList d 

balance :: BTree a -> BTree a 
balance t = build (toList t ) 

build :: ([a],Int) -> BTree a 
build ([], 0) = Empty 
build (l,n) = let a = n `div` 2 
                  (l1,(x:l2)) = splitAt a l 
                    in Node x (build (l1,a))(build (l2,n-a-1))








