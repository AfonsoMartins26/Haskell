module PF50 where
import Data.List ()


--1
enumFromTo' :: Int -> Int ->[Int] 
enumFromTo' start end 
   | start < end = start : enumFromTo' (start+1) end 
   | otherwise = []

--2
enumFromThenTo' :: Int -> Int-> Int -> [Int]
enumFromThenTo' start next end 
  |start > end && next >= start || start < end && next < start = []
  |otherwise = start : enumFromThenTo' next (2*next - start) end 

--3
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l 
(+++) (h:t) l = h: (+++) t l 

--4
(!!!) :: [a] -> Int -> a
(!!!) (h:t) n 
  | n == 0 = h 
  | otherwise = (!!!) t (n-1)

-- 5 
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse t ++[h] 

--6 
take' :: Int -> [a] -> [a] 
take' _ [] = []
take' n (h:t) 
  | n <= 0 = []
  |otherwise = h: take' (n-1) t 
 
--7
drop' :: Int -> [a] -> [a]
drop' _ [] = [] 
drop' n (h:t) 
 | n <= 0 = (h:t) 
 |otherwise = drop' (n-1) t 

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = [] 
zip' (h1:t1) (h2:t2) = (h1,h2): zip' t1 t2 

--9 
replicate' :: Int -> a ->[a]
replicate'  0 _ = []
replicate' n x = x:replicate' (n-1) x 

--10
intersperse' :: a -> [a] ->[a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' n (h:t) = h:n:intersperse' n t 

--11 
group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = insere h (group t)

insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)


--12
concat' :: [[a]] -> [a]
concat' [[]] = []
concat' (h:t) = h ++ concat' t 

--13 ela não dá como no exemplo 
inits :: [a] -> [[a]]
inits [] = [[]]
inits l =  inits (init l) ++ [l]

--14 
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tail (tails l) 

--15 
heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t 
heads (h:t) = head h : heads t

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = subTotal h + total t
    where subTotal :: [a] -> Int
          subTotal [] = 0
          subTotal (h:t) = 1 + subTotal t


--17
fun' :: [(a,b,c)] -> [(a,c)]
fun' [] =[]
fun' ((x,y,z):t)=(x,z): fun' t 

--18
cola' :: [(String,b,c)] -> String
cola' [] = ""
cola' ((nome,b,c):t) = nome ++  cola' t

--19
idade' :: Int -> Int -> [(String,Int)] -> [String]
idade' _ _ [] = []
idade' ano years ((nome,adn):t) 
   | ano - adn >= years = nome : idade' ano years t 
   |otherwise = idade' ano years t 

--20
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n 1 = [1]
powerEnumFrom' n m
    | m > 1 = powerEnumFrom' n (m - 1) ++ [n^(m-1)]
    | otherwise = []

--21 
isPrime :: Int -> Bool
isPrime n = n>= 2 && primeCheck n 2 


primeCheck :: Int  -> Int -> Bool 
primeCheck n m 
  | m*m <=n && mod n m == 0 = True 
  |otherwise = False 

--22
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf'  _ [] = False 
isPrefixOf' [] _ = True 
isPrefixOf' (h1:t1) (h2:t2) 
  |h1 == h2 && isPrefixOf' t1 t2 = True 
  |otherwise = False 

--23 
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True 
isSuffixOf' _ [] = False 
isSuffixOf' (h1:t1) (h2:t2) 
   | h1 /= h2 && isSuffixOf' t1 t2 = True 
   |otherwise = False 

--24 
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' _ [] = False 
isSubsequenceOf' [] _ = True 
isSubsequenceOf' (h1:t1) ( h2:t2) 
  | h1== h2 && isSubsequenceOf' t1 t2 || isSubsequenceOf' t1 t2 = True 
  |otherwise = False 

--25 
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x l = elemIndicesAux x l 0


elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux x (h:t) i 
    | x == h = i : elemIndicesAux x t (i+1)
    | otherwise = elemIndicesAux x t (i+1)

--26
nub' :: Eq a => [a] -> [a]
nub'  [] = [] 
nub' (h:t) 
  | h `elem` t =nub' t 
  |otherwise =h: nub' t 


  --27
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = [] 
delete' n (h:t) 
  |n == h = t 
  |otherwise = h:delete' n t  

--28
(\\\) :: Eq a => [a] -> [a] -> [a] 
(\\\) l [] = l
(\\\) [] _ = [] 
(\\\) l (h:t) = (\\\) (delete' h l) t  

--29
union' :: Eq a => [a] -> [a]-> [a] 
union' l [] = l
union' l (h:t) 
   | h `elem` l = union' l t 
   |otherwise = h : union' l t 

--30 
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (h:t) l
    | h `elem` l = h : intersect' t l
    | otherwise = intersect' t l

--31
insert' :: Ord a => a -> [a]-> [a] 
insert' x [] = [x]
insert' x (h:t) 
  | x>h =h : insert' x t 
  |otherwise = x: h :t 

--32 
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ (if null t then "" else " ") ++ unwords' t

--33
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

---34 
pMaior' :: Ord a => [a] -> Int
pMaior' [_] =0 
pMaior' (h:t)
  | h >= ( t !! x) =0 
  |otherwise = 1 + x 
   where x = pMaior' t 

--35 
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing 
lookup' n ((a,b):t) 
  |n == a = Just b
  |otherwise = lookup' n t  

  --36 
preCrescente' :: Ord a => [a] -> [a] 
preCrescente' [] = [] 
preCrescente' (h:s:t) 
  | h < s = h: preCrescente' (s:t)
  |otherwise = [h]


--37 
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = insert' h (iSort' t)


--38 
menor' :: String -> String -> Bool
menor' _ "" = False 
menor' "" _ = True 
menor' (h:t)(h1:t1) 
  |h<h1 = True 
  |h==h1 = menor' t t1 
  |otherwise = False 

--39 
elemMSet' ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' a ((x,_):xs) = a == x || elemMSet' a xs

--40 
converterMSet' :: [(a,Int)] -> [a]
converterMSet' [] = []
converterMSet' ((h,1):t)= h : converterMSet' t 
converterMSet' ((h,x):t)= h : converterMSet' ((h,(x-1)):t)

--41 
insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' x [] = [(x,1)]
insereMSet' x ((h,n):t)
   |x == h = ((h,n+1):t)
   |otherwise = ((h,n):insereMSet' x t)

--42 
removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' _ [] = [] 
removeMSet' n ((h,x):t) 
  |n== h = if x <= 1 then t else (h,(x-1)):t
  |otherwise = removeMSet' n t

--43 
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = [] 
constroiMSet (l:ls) = insereMSet' l (constroiMSet ls)

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
catMaybes (Nothing : t) = catMaybes t 
catMaybes (Just x :t) = x : catMaybes t 

--46 
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi, yi) (xf, yf) 
    | xi < xf = Este : caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste : caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte : caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul : caminho (xi, yi - 1) (xf, yf)
    |otherwise = [] 


--47 
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False 
hasLoops posi movs = hasloopsaux' posi posi movs


corremov :: (Int,Int) -> Movimento -> (Int,Int)
corremov (x,y) Norte = (x,y+1)
corremov (x,y) Sul = (x,y-1)
corremov (x,y) Este = (x+1,y)
corremov (x,y) Oeste = ( x-1,y)

hasloopsaux' :: (Int,Int) -> (Int,Int) -> [Movimento] -> Bool
hasloopsaux' _ _  [] = False 
hasloopsaux' inicial atual (h:t) 
  |inicial == x = True
  |otherwise = hasloopsaux' inicial x t 
  where x = corremov atual h 

-- 48 
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0 
contaQuadrados ((Rect (x1,y1)(x2,y2)):t)
  | abs (y2-y1) == abs (x2-x1) = 1 + contaQuadrados t 
  |otherwise = contaQuadrados t 

-- 49 
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

--50 
data Equipamento = Bom | Razoavel | Avariado
    deriving Show
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0 
naoReparar (h:t) = reparar h + naoReparar t 


reparar :: Equipamento -> Int
reparar Bom = 1 
reparar Razoavel = 1
reparar Avariado = 0 