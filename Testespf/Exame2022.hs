module TestePf.Exame2022 where
import Data.List
import System.Random 
import Control.Monad

--2 de Fevereiro de 2022

--1 
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x
    |n<0 = []
    |otherwise=  x : replicate' (n-1) x

--2
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (h:t) l
  | h `elem` l = h : intersect' t l
  |otherwise = intersect' t l

--3 
data LTree a = Tip a | Fork (LTree a) (LTree a)
data FTree a b = Leaf a | No b (FTree a b) (FTree a b)

conv :: LTree Int -> FTree Int Int
conv (Tip x) = Leaf x
conv (Fork l r) = snd (convAux (Fork l r))

convAux :: LTree Int -> (Int, FTree Int Int)
convAux (Tip x) = (x, Leaf x)
convAux (Fork l r) = (s, No s ll rr)
    where (sl, ll) = convAux l
          (sr, rr) = convAux r
          s = sl + sr

-- 4
type Mat a = [[a]]

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup = all (all (== 0) . uncurry take) . zip [0..]

--5 
data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

-- (a) 
instance Show SReais where 
  show :: SReais -> String 
  show (AA x y) = "]" ++ show x ++ ","++ show y  ++ "["
  show (FF x y) = "[" ++ show x ++ ","++ show y ++  "]"
  show (AF x y) = "]" ++ show x ++ ","++ show y  ++ "]"
  show (FA x y) = "[" ++ show x ++ ","++ show y  ++ "["
  show (Uniao x y) = "("++ show x ++ "U"++show y ++ ")"

-- (b)
tira :: Double -> SReais -> SReais
tira v (AA x y) = if x < v && y > v  
                  then (Uniao (AA x v) (AA v y)) 
                  else AA x y 
tira x (FF a b)
    | x > a && x < b = Uniao (FA a x) (AF x b) 
    | x == a = AF a b
    | x == b = FA a b
    | otherwise = FF a b
tira x (AF a b)
    | x > a && x < b = Uniao (AA a x) (AF x b) 
    | x == b = AA a b
    | otherwise = AF a b
tira x (FA a b)
    | x > a && x < b = Uniao (FA a x) (AA x b) 
    | x == a = AA a b
    | otherwise = FA a b
tira x (Uniao a b) = Uniao (tira x a) (tira x b)

--6
func :: Float -> [(Float,Float)] -> [Float]
func x l = map snd (filter ((>x) . fst) l)

func' :: Float -> [(Float,Float)] -> [Float] 
func' _ [] =[] 
func' x ((h,h1):t)
  | h >= x = h1 :  func' x t
  | otherwise = func' x t 

-- 7 
subseqSum :: [Int] -> Int -> Bool
subseqSum [] _ = False 
subseqSum l n  = any((==n).sum) (inits l) || subseqSum (tail l) n 

subseqSum' :: [Int] -> Int -> Bool
subseqSum' [] _ = False
subseqSum' (x:xs) k = subseqSumStartingFrom (x:xs) k || subseqSum' xs k

subseqSumStartingFrom :: [Int] -> Int -> Bool
subseqSumStartingFrom [] k = k == 0
subseqSumStartingFrom (x:xs) k = subseqSumStartingFrom xs (k - x) || subseqSumStartingFrom xs k
--assim se houverem valores na lista que anulam k sem que se repita elementos da lista  
--quer dizer que existe uma sub-sequencia da lista que a sua soma seja igual a k

--8
jogo :: Int -> (Int, Int) -> IO ()
jogo t (a, b) = do
  putStr "Digite um numero: "
  s <- getLine
  l <- sequence (replicate t (randomRIO (a, b)) ) -- sequence  muda uma lista monad para uma lista normal 
  if subseqSum l (read s)
    then putStrLn "A propriedade é válida"
    else putStrLn "A propriedade é inválida"


jogo' :: Int -> (Int, Int) -> IO ()
jogo' n (a,b) = 
    sequence (replicate n (randomRIO (a,b)))
    >>= (\seq -> 
        print seq
        >> putStrLn "Introduza um numero:"
        >> readLn
        >>= (\s ->
            if subseqSum seq s then
                putStrLn "A propriedade verifica-se."
            else
                putStrLn "A propriedade nao se verifica."
            )
    )


     

