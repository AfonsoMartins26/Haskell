module Pf50 where


--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end
    | start > end = []
    | otherwise = start : enumFromTo' (start + 1) end

--2
enumFromThento' :: Int -> Int -> Int -> [Int]
enumFromThento' start next end
    | start > end && next >= start || start < end && next < start = []
    | otherwise = start : enumFromThento' next (2 * next - start) end

--3
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h:t) l = h : (+++) t l

--4
(!!!) :: [a] -> Int -> a
(!!!) (h:_) 0 =h
(!!!) (_:t) n = (!!!) t (n-1)

--5 > reverse [10,20,30] [30,20,10]
reverse' :: [a] -> [a]
reverse' [] =[]
reverse' (h:t) =reverse' t ++ [h]

--6
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (h:t)
    | n <= 0 = []
    | otherwise = [h] ++ take' (n - 1) t

 --7
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (h:t)
    | n <= 0 = h : t
    | otherwise = drop' (n - 1) t

--8 
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] =[]
zip' [] _ =[]
zip (h:t) (h1:t1) =(h,h1) : zip' t t1      -- ou posso fazer zip (h:t) (h1:t1) =(h,h1) : zip [t] [t1]

--9 
replicate' :: Int -> a -> [a]
replicate' 0 _ =[]
replicate' n x
         |n<=0 =[]
         |otherwise = x : replicate' (n-1) x

--10 não sei pq que está a dar a erro
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h]=[h]    --se a lista for só de um elemento não há maneira de meter no meio então ẽ dada a propria lista
intersperse' x (h:t) = h:x : intersperse' x t

--11
group :: Eq a => [a] -> [[a]]
group []= []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r --não entendi nada
    | otherwise = [h] : r
    where r = group t

--12
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--13
inits :: [a] -> [[a]]
inits [] = [] 
inits l = inits (init l) ++ [l]

--14
tails :: [a] -> [[a]]
tails [] = []
tails l = [l] ++ tails ( tail l) 
--15
heads :: [[a]] -> [a]
heads [] =[]
heads ([]:t) = heads t
heads [h:t]=head [h]:heads [t]

--16
total :: [[a]] -> Int --length
total [] = 0
total [h:t] = (length [h]) + total [t]

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = (x,z):fun t

--18
cola :: [(String,b,c)] -> String
cola [] =""
cola ((h,x,y):t) = h  ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano n ((nome ,years):t)
            |(years -ano)>= n =nome :idade ano n t
            | otherwise = idade ano n t

--20 
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 0 = [1]
powerEnumFrom n m
    |  m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
    | otherwise = []

--21   
isPrime :: Int -> Bool
isPrime n = n>=2 && primeCheck n 2

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)

--22
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (h:t) (h':t') = if h == h' && isPrefixOf' t t'
                            then True 
                            else False 

--23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False 
isSuffixOf (h:t) ( h1:t1) =  if h /= h1 && isSuffixOf t t1 
                            then True 
                            else False 


--24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf _ [] = True
isSubsequenceOf [] _ = False
isSubsequenceOf (h:t)(h1:t1) = h ==h1 && isSubsequenceOf t t1 || isSubsequenceOf (h:t) t1

--25 
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = elemIndicesAux x l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux x (h:t) i -- a variável `i` indica-nos qual o índice do elemento da lista que estamos a consultar
    | x == h = i : elemIndicesAux x t (i+1)
    | otherwise = elemIndicesAux x t (i+1) 


--26
nub :: Eq a => [a] -> [a]
nub [] =[]
nub (h:t) = if h `elem` t
            then nub t
            else h : nub t

--27
delete' :: Eq a => a -> [a] -> [a]
delete'  _ [] = []
delete' x (h:t)
       | h == x = t
       |otherwise = h: delete' x t

--28
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\)  l [] = l
(\\\) [] _ = []
(\\\) l (h:t) = (\\\) ( delete' h l) t

--29
union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union l (h:t)
    |  h `elem` l = union l t
    |  otherwise = union (l ++ [h]) t

-- 30 
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] l = []
intersect (h:t) l
   | h `elem` l = h :intersect t l
   |otherwise = intersect t l

--31
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t)
  |x > h =h: insert x t
  |otherwise = x:h:t

--32
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ unwords' t  ------pq que não é assim e e como é que era suposto saber fazer aquilo ????

--33
unlines' :: [String] -> String
unlines' [] =""
unlines' (h:t) = h ++ "\n" ++ (unlines' t)

--34 

pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t)
    | h >= (t !! x) = 0
    | otherwise = 1 + x
    where x = pMaior t

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' e ((a,b):t)
    | e == a = Just b
    | otherwise = lookup' e t


--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t)
  |s >= h = h : preCrescente (s:t)
  |otherwise  = [h]

--37 
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)   --percebi mais ou menos

--38
menor :: String -> String -> Bool  -- exerplo conseguimos ver que "" é maior do que qualquer letra neste caso o "u"
menor _ "" = False
menor "" _ = True
menor (h:t) (h1:t1)
   |h< h1 = True
   |h==h1 = menor t t1
   |otherwise = False

--39
elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMset n (h:t)
     | n `elem` h = True
     | otherwise = elemMset n t

--40 
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converterMSet ((a,1):t) = a : converterMSet t 
converterMSet ((a,n):t) =a : converterMSet ((a,(n-1)):t)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,n):t) 
    | a == x =(x,n+1) : t
    |otherwise = (x,n) : insereMSet a t 

--42 
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,n):t) 
     |x == a =  if n >= 1 then(a,n-1):t  else t 
     |otherwise = (a,n) : removeMSet x t 

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

-- tirar duvida com o stor 

{-partitionEithers [(Left n):t] = (n : (partitionEithers t))
partitionEithers [(Right n):t] = (n :(partitionEithers t))-}

--45 
catMaybes :: [Maybe a] -> [a] 
catMaybes [] = []
catMaybes ((Just a):t) = a : catMaybes t 
catMaybes ((Nothing):t) = catMaybes t 

--46 
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) 
   |x1< x2 = Este : caminho (x1+1,y1)(x2,y2)
   |y1< y2 = Norte : caminho (x1,y1+1)(x2,y2) 
   |x1>x2  = Oeste : caminho (x1 -1,y1)(x2,y2)
   |y1>y2  = Sul : caminho (x1,y1-1)(x2,y2)

--47 muito trabalho e dificil
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (Norte:t) = posicao (x, y + 1) t
posicao (x, y) (Sul:t) = posicao (x, y - 1) t
posicao (x, y) (Este:t) = posicao (x + 1, y) t
posicao (x, y) (Oeste:t) = posicao (x - 1, y) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)


--48 
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] =0 
contaQuadrados ((Rect (x1,y1)(x2,y2)):t)
   | abs (x2-x1) == abs (y2-y1) = 1 + contaQuadrados t 
   |otherwise = contaQuadrados t 

--49 
areaTotal :: [Rectangulo] -> Float 
areaTotal [] = 0 
areaTotal ((Rect (x1,y1)(x2,y2)):t) = abs (x2-x1)*abs (y2-y1) + areaTotal t  

--50
data Equipamento = Bom | Razoavel | Avariado
    deriving Show 


naoReparar :: [Equipamento] -> Int
naoReparar [] = 0 
naoReparar (Bom:t) = 1 + naoReparar t 
naoReparar (Razoavel:t) = 1 + naoReparar t 
naoReparar (Avariado:t) = 0 + naoReparar t  
