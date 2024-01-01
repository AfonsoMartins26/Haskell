module TestePf.Exame2023 where

-- 30 de Janeiro de 2023

type MSet a = [(a,Int)]

--1
-- (a) 
converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((h,h1):t)= replica h h1 ++ converteMSet t 

replica :: a -> Int  -> [a]
replica h 0 = []
replica h 1 = [h]
replica h n  = h : replica h (n-1) 

-- (b)
removeMSet :: Eq a => a -> MSet a -> MSet a
removeMSet _ [] = [] 
removeMSet x ((h,h1):t) 
    | x ==h  =  if h1 > 1   
                then  (h,h1-1): removeMSet x t  
                else t 
    | otherwise = (h,h1):removeMSet x t

-- (c)
uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a
uniaoMSet s1 [] = s1 
uniaoMSet [] s2 = s2
uniaoMSet ((x,n1):t1) ((y,n2):t2) 
      | x == y = (x,n1+n2): uniaoMSet t1 t2 
      | otherwise = (x,n1):(y,n2): uniaoMSet t1 t2 

--2 
type Posicao = (Int,Int)
data Movimento = Norte | Sul | Este | Oeste
data Caminho = C Posicao [Movimento]

instance  Eq (Caminho) where
  (==) :: Caminho -> Caminho -> Bool  
  (C p1 lista1) == (C p2 lista2) = p1 == p2 && (length lista1) == (length lista2)


--3 
func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x >10) l)

func' :: [[Int]] -> [Int]
func' [[]] = []
func' (h:t) 
    | sum h > 10 = h ++ func' t 
    |otherwise = func' t 

--4 
data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop

p1 :: Prop
p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))


-- (a)
eval :: [(String, Bool)] -> Prop -> Bool
eval l (Var x) = True 
eval l (Not p) = not (eval l p)
eval l (And p q) = eval l p && eval l q
eval l (Or p q) = eval l p || eval l q

-- (b)
nnf :: Prop -> Prop
nnf (Var x) = Var x
nnf (Not (Var x)) = Not (Var x)
nnf (Not (Not p)) = nnf p
nnf (Not (And p q)) = Or (nnf (Not p)) (nnf (Not q))
nnf (Not (Or p q)) = And (nnf (Not p)) (nnf (Not q))
nnf (And p q) = And (nnf p) (nnf q)
nnf (Or p q) = Or (nnf p) (nnf q) 
