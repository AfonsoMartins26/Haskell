module Pf5 where
import Data.List
data BTree a = Empty 
             | Node a (BTree a )(BTree a)
    deriving (Show)
 

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False  
any' f (h:t) 
   | f h = f  h 
   |otherwise = any' f t 

any'' :: (a -> Bool) -> [a] -> Bool
any'' f [] = False 
any'' f (h:t) = f h || any'' f t 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _ _ _ = []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = [] 
takeWhile' f (h:t) 
  | f h = h : takeWhile' f t 
  | otherwise  = [] 

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = [] 
dropWhile' f ( h:t) 
  |f h = dropWhile' f t 
  |otherwise = [] 

--span p l = (takeWhile p l, dropWhile p l)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[]) 
span' f (h:t) 
   | f h = (h:takeWhile' f t , dropWhile' f t )
   | otherwise = ([],[])

span'' :: (a-> Bool) -> [a] -> ([a],[a])
span'' _ [] = ([],[]) 
span'' p (x:xs) 
     | p x = (x:a,b) 
     | otherwise  = ([],x:xs) 
     where (a,b) = span'' p xs 

type Mat a = [[a]]

dimOK' :: Mat a -> Bool 
dimOK' [l] = True 
dimOK'  (h:t)= let larg  = length h 
         in dimOK' larg t 
    where dimOK'  _ [] = True 
          dimOK' larg (l:ls) 
            | length l == larg = dimOK' larg ls 
            | otherwise = False 

dimOK'' :: Mat a -> Bool 
dimOK'' m = 
     length (nub ( map length m )) == 1 

dimMat :: Mat a -> (Int,Int) 
dimMat m = (length ( head m ), length m )

addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat []  [] = []
addMat (x:xs) (y:ys) = zipWith' (+) x y : addMat xs ys 


m = [[1,2,3],[0,4,5],[0,0,6]] 

a

testabalanca :: BTree a -> Bool 
testabalanca Empty = True 
testabalanca (Node x e d) = abs(altura e - altura d) <= 1 && 
                            testabalanca e && testabalanca d 

