module TestePf.Exame2020 where

-- 25 de janeiro de 2020

--1 
-- (a)
inits:: [a] -> [[a]] 
inits [] = [[]]
inits l=  inits (init l) ++ [l]

-- (b)
isPrefixOf:: Eq a => [a] -> [a] -> Bool
isPrefixOf  _ [] = False 
isPrefixOf [] _ = True 
isPrefixOf (h:t)(h1:t1) = h == h1 && isPrefixOf t t1 


-- 2
data BTree a = Empty
      | Node a (BTree a) (BTree a)
        deriving Show

-- (a)
folhas :: BTree a -> Int
folhas Empty =  0
folhas (Node x Empty Empty) = 1 
folhas (Node x l r ) = folhas l + folhas r 

-- (b)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []  
path [] _ = []    
path (False:t) (Node x l r) = x : path t l  
path (True:t) (Node x l r) = x : path t r  

-- 3
type Polinomio = [Coeficiente]
type Coeficiente = Float

-- (a)
valor :: Polinomio -> Float -> Float
valor l x = valorAux l x 0
    where valorAux [] x 0 = 0
          valorAux (h:t) x pos
            | h == 0 = valorAux t x (pos + 1)
            | otherwise = h * (x ^ pos) + valorAux t x (pos + 1)

-- (b)
deriv :: Polinomio -> Polinomio
deriv l = tail ( derivAux l 0)
    where derivAux [] _ = []
          derivAux (h:t) pos = (pos * h) : derivAux t (pos + 1)

-- (c)
soma :: Polinomio -> Polinomio -> Polinomio
soma = zipWith (+) 

--4 
type Mat a = [[a]]
ex = [[1,4,3,2,5], 
      [6,7,8,9,0],
      [3,5,4,9,1]
      ]  

-- (a) 
quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = [] 
quebraLinha (x:xs) l = take x l : quebraLinha xs (drop x l) 
 
-- (b) 
fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (h:t) l m = quebraLinhas l (take h m) ++ fragmenta t l (drop h m)

quebraLinhas :: [Int] -> Mat a -> [Mat a]
quebraLinhas [] _ = []
quebraLinhas (h:t) m = map (take h) m : quebraLinhas t (map (drop h) m)

-- (c)
geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int)
geraMat (0,_) _ = return []
geraMat (x,y) (a,b) = do
    h <- geraLinha y (a,b)
    t <- geraMat (x - 1,y) (a,b)
    return (h:t)

geraLinha :: Int -> (Int,Int) -> IO [Int]
geraLinha 0 _ = return []
geraLinha x (a,b) = do
        h <- randomRIO (a,b)
        t <- geraLinha (x - 1) (a,b)
        return (h:t) 