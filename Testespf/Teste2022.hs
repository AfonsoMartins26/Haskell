module TestePf.Teste2022 where
import Data.List
import System.Random
-- 12 de janeiro de 2022 
--1 
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t)(h1:t1) = (h,h1): zip' t t1 

--2
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente(h:s:t)
   |s >=h = h : preCrescente (s:t) 
   |otherwise = preCrescente (s:t)

--3 
amplitude :: [Int]-> Int
amplitude [] = 0 
amplitude l = (maximo l) - minimo l 

maximo :: [Int]-> Int
maximo [x] =x 
maximo (h:t)
  | h >= head t = maximo (h:(tail t))
  |otherwise = maximo t  

minimo :: [Int]-> Int
minimo [x] =x 
minimo (h:t)
  | h <= head t = minimo (h:(tail t))
  |otherwise = minimo t  


--4 
type Mat a = [[a]]
soma:: Num a => Mat a -> Mat a -> Mat a
soma = zipWith . zipWith $ (+) 

--5 
type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

instance Show Agenda where 
  show :: Agenda -> String 
  show Vazia = ""
  show (Nodo (nome,tlfns) l r) = 
    show l ++ nome ++ " " ++ intercalate "/" ( map show tlfns) ++ "\n" ++ show r 


--6 -- não etendi 
randomSel :: Show a => Int -> [a] -> IO [a]
randomSel _ [] = return []
randomSel 0 _ = return []
randomSel n l = 
    randomRIO (1, length l)
    >>= (\randomN -> 
        fmap (l !! (randomN - 1) :) (randomSel (n - 1) (take (randomN - 1) l ++ drop randomN l))
    )

--7  não sei 
organiza :: Eq a => [a] -> [(a,[Int])]
organiza = foldr (\a -> insere a . map (\(c,is) -> (c,map (+1) is))) []

insere :: Eq a => a -> [(a,[Int])] -> [(a,[Int])]
insere x [] = [(x,[0])]
insere x ((c,is):t)
    | x == c = (c, 0 : is) : t
    | otherwise = (c,is) : insere x t

--8 
func :: [[Int]] -> [Int]
func [[]] = []
func (h:t) 
     |sum h > 10 = h ++ func t 
     |otherwise = func t 

--9 
data RTree a = R a [RTree a]
type Dictionary = [ RTree (Char, Maybe String) ]

d1 = [R ('c',Nothing) [
        R ('a',Nothing) [
            R ('r', Nothing) [
                R ('a',Just "...") [
                    R ('s', Just "...") [] ],
                R ('o',Just "...") [],
                R ('r',Nothing) [
                    R ('o',Just "...") [] ]
     ]  ]   ]   ]

insere' :: String -> String -> Dictionary -> Dictionary
insere' [x] desc dict = insereFim x desc dict
insere' (h:t) desc [] = [R (h,Nothing) (insere' t desc [])]
insere' (h:t) desc (R (a,b) l:r) 
    | h == a = R (a,b) (insere' t  desc l) :r 
    |otherwise = R (a,b) l : insere' (h:t) desc r

insereFim :: Char -> String -> Dictionary -> Dictionary
insereFim x desc [] = [ R (x,Just desc)[]]
insereFim x desc (R (a,b)l:r) 
     | x == a = R (a,Just desc) l:r  
     |otherwise = R (a,b) l : insereFim x desc r  -- metemos para inserir na parte da direita pois na parte esquerda vai continuar a palavra 
                                                  --  assim inserimos x no resto da arvore para formar uma palavra nova  
