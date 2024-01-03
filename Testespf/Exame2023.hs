module TestePf.Exame2023 where

-- 30 de Janeiro de 2023

type MSet a = [(a,Int)]

--1
-- (a) 
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):xs) = x : converteMSet xs
converteMSet ((x,n):xs) = x : converteMSet ((x,n-1) : xs)

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
uniaoMSet l a = uniaoAux l (converteMSet a)

uniaoAux :: Eq a => MSet a -> [a] -> MSet a
uniaoAux l [] = l
uniaoAux l [x] = insereMSet x l
uniaoAux l (x:xs) = uniaoAux (insereMSet x l) xs

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,n):xs) = if x == a then (a,n+1) : xs else (a,n) : insereMSet x xs

--2 
type Posicao = (Int,Int)
data Movimento = Norte | Sul | Este | Oeste
data Caminho = C Posicao [Movimento]

instance  Eq Caminho where
  (==) :: Caminho -> Caminho -> Bool
  c1@(C p1 list1)==  c2@(C p2 list2) = p1 == p2 && length list1 == length list2 && posChegada c1 == posChegada c2

posChegada :: Caminho -> Posicao
posChegada (C (a,b) [] ) = (a,b)
posChegada (C (a,b) (Norte:xs)) = posChegada (C (a,b+1) xs)
posChegada (C (a,b) (Sul:xs)) = posChegada (C (a,b-1) xs)
posChegada (C (a,b) (Este:xs)) = posChegada (C (a+1,b) xs)
posChegada (C (a,b) (Oeste:xs)) = posChegada (C (a-1,b) xs)

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
eval [(a,True)] (Var x) = True
eval [(a,False)] (Var x) = False 
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
