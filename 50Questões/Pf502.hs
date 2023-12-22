module Pf502 where
import Data.List
import Data.Binary.Get (label)



--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y 
    |x < y = []
    | otherwise = x: enumFromTo' (x + 1) y 

--2 
enumFromThenTo' :: Int -> Int-> Int -> [Int] 
enumFromThenTo' start next end 
   |start < next && next > end || start > next && next < end = []
   | otherwise = start : enumFromThenTo' next (2*next - start) end 

--3 
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) l [] = l 
(+++) (h:t) l = h : (+++) t l 

--4 
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h 
(!!!) (h:t) n = (!!!) t (n-1) 

--5 
reverse' :: [a] -> [a]
reverse' [] = [] 
reverse' (h:t) = reverse' t ++ [h]  

--6 
take' :: Int -> [a] -> [a]
take' _ [] = [] 
take'  n (h:t)  
     |n <= 0 = []
     | otherwise = h: take' (n-1) t 

--7 
drop' :: Int -> [a] -> [a]
drop' _ [] = [] 
drop' n (h:t) 
    | n <= 0 = (h:t)
    |otherwise = h: drop' (n-1) t 

--8 
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = [] 
zip' [] _ = [] 
zip' (h:t) (h1:t1) = (h,h1) : zip' t t1 

-- 9 
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x 
    | n < 0 = [] 
    |otherwise = x : replicate' (n-1) x 

-- 10 
interperse' :: a -> [a] -> [a]
interperse' _ [] = [] 
interperse' _ [h] = [h]
interperse' x (h:t) = h:x :  interperse' x t 

-- 11 
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group' t

--12
concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [h:t] = [h] ++ concat' [t] 

--13 
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--14 
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails'( tail l )

--15 
heads :: [[a]] -> [a]
heads [[]] = [] 
heads ([]:t)= heads t 
heads (h:t) = head [h] ++ heads t  

--16
total :: [[a]] -> Int
total [[]] = 0 
total (h:t) = somal h + total t 


somal :: [a] -> Int
somal [] = 0 
somal (h:t) = 1 + somal [t] 


--17 
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = ((x,z): fun t  )

-- 18 
cola :: [(String,b,c)] -> String 
cola [] = "" 
cola ((x,y,z) : t ) = x ++ cola t 

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade  _ _ [] = []
idade ano idadex ((x,y):t)
      | y - ano >= idadex = (x:idade ano idadex t)

-- 20 
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = [1] 
powerEnumFrom n m 
         | m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
         | otherwise = []

--21 
isPrime :: Int -> Bool
isPrime n = n >= 2 && isPrimeCheck' n 2


isPrimeCheck' :: Int -> Int -> Bool
isPrimeCheck' n m 
    | m^2 > n = True 
    | mod n m  ==0 = True 
    | otherwise = isPrimeCheck' n (m + 1) 

--22 
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False 
isPrefixOf' (h:t) (h1:t1) 
          | h == h1 && isPrefixOf' t t1 = True 
          | otherwise = False 

-- 23  
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True 
isSuffixOf' _ [] = False 
isSuffixOf' (h:t) (h1: t1 ) 
       | h /= h1 && isSuffixOf' t t1 = True 
       | otherwise = False 

--24 
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True 
isSubsequenceOf' _ [] = False 
isSubsequenceOf' (h:t) (h1:t1) 
      | h == h1 && isSubsequenceOf' t t1 || isSubsequenceOf' (h:t) t1 = True 
      |otherwise = False 

-- 25
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = elemIndicesAux x l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux x (h:t) i 
    | x == h = i : elemIndicesAux x t (i+1)
    | otherwise = elemIndicesAux x t (i+1)

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if h `elem` t  
            then nub' t 
            else h: nub' t 

--27 
delete' :: Eq a => a -> [a] -> [a]
delete'  _ [] = [] 
delete' n (h:t) 
   | n == h = t 
   |otherwise = h : delete' n t 

--28 pq que não pode ser assim
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) l [] = l
(\\\) [] _ = [] 
(\\\) (h:t) (h1:t1) 
      | h==h1 = (\\\) t t1 
      | otherwise = h: (\\\) t t1 


{-remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove l (h:t) = remove (delete h l) t-}


--29
union'' :: Eq a => [a] -> [a] -> [a]
union'' [] l = l 
union'' l [] = l 
union' l (h:t)
    | h `elem` l = union' l t
    | otherwise = union' (l ++ [h]) t

--30 
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = [] 
intersect' l (h:t) 
    | h `elem` l = h: intersect' l t 
    | otherwise = intersect' l t 

-- 31 
insert' :: Ord a => a -> [a] -> [a]
insert' n [] =[n]
insert' n (h:t) 
   |n > h = h : insert' n t 
   |otherwise = n : h : t 

-- 32 
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ (if null t then "" else " ") ++ unwords' t

--33
unlines' :: [String] -> String
unlines' [] = [] 
unlines' (h:t) = h ++ "\n" ++ unlines' t 

--34 
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t) 
    | h >= ( t !!! x) = 0 --- nós fazemos (t !!! x ) para sabermos o elemento maior da lista t  dado que pMaior t dá a posicao do amior elemento e !!! dá o elemento na posicao pMaior t 
    | otherwise = 1 + x
   where x = pMaior  t

--35 
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' n ((x,y):t)  
     | n == x = Just y
     |otherwise = lookup' n t 


--36 
preCrescente :: Ord a => [a] -> [a] 
preCrescente [] = [] 
preCrescente [x] = [x]
preCrescente (h:s:t)
   |s  >= h = h : preCrescente (s:t)
   |otherwise = [h] 

-- 37 
iSort :: Ord a => [a] -> [a]
iSort [] = [] 
iSort (h:t) = insert' h (iSort t) --- vamos organizar t de forma crescente e depois inserir o h de forma a continuar uma lista cresecente 

--38 ez 
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True 
menor (h:t) (h1:t1) 
   |h < h1 = True 
   |h == h1 = menor t t1 
   |h > h1  = False 

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False 
elemMSet a ((x,_):xs) = a == x || elemMSet a xs


elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False 
elemMSet' a ((h,x):t) 
     |a == h = True 
     |otherwise = False 

--40

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = [] 
converterMSet  ((h,1):t) = h : converterMSet t 
converterMSet  ((h,n):t) = h : converterMSet ((h,n-1):t)

--41 
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)] 
insereMSet a ((x,n):t) = if a ==x then ((x,n+1):t) else (x,n): insereMSet a t 

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((h,x):t) 
   | a == h = if x > 1 then ((h,x-1):t) else t 
   | otherwise =  (h,x) : removeMSet a t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = [] 
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--44 
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a : as,bs)
    where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b : bs)
    where (as,bs) = partitionEithers t

--45 
catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (Just a : t) =  a: catMaybes t
cattMaybes (Nothing : t) = catMaybes t 

--46 
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1)(x2,y2) 
   |x1 < x2 = Este : caminho (x1+1,y1)(x2,y2)
   |x1 > x2 = Oeste :caminho (x1-1,y1)(x2,y2)
   |y1 > y2 = Norte : caminho (x1,y1+1)(x2,y2)
   |y1 < y2 = Sul : caminho (x1,y1-1)(x2,y2)


--47 

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False 
hasLoops posi movs = hasloopsaux posi posi movs


hasloopsaux :: (Int,Int) -> (Int,Int) -> [Movimento] -> Bool
hasloopsaux _ _  [] = False 
hasloopsaux inicial atual (h:t) 
  |inicial == x = True
  |otherwise = hasloopsaux inicial x t 
  where x = corremov atual h 

corremov :: (Int,Int) -> Movimento -> (Int,Int)
corremov (x,y) Norte = (x,y+1)
corremov (x,y) Sul = (x,y-1)
corremov (x,y) Este = (x+1,y)
corremov (x,y) Oeste = ( x-1,y)

--48

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0 
contaquadrados (h:t) 
   | eQuadrado h = 1 + contaQuadrados t 
   | otherwise = contaQuadrados t



eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x1,y1) (x2,y2)) = if abs (x2 -x1) == abs (y2 -y1) then True
                                 else False 

--49 

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0 
areaTotal ((Rect (x1,y1)(x2,y2)):t) = abs ( x2 -x1) * abs( y2-y1) + areaTotal t 


--50 
data Equipamento = Bom | Razoavel | Avariado
    deriving Show


naoReparar :: [Equipamento] -> Int
naoReparar [] = 0 
naoReparar (h:t) 
 | reparar h = 1 + naoReparar t 
 |otherwise = naoReparar t


reparar :: Equipamento -> Bool 
reparar Bom = True 
reparar Razoavel = True
reparar Avariado = False 