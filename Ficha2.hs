module Pf2 where
import Data.Char
import Distribution.Compat.CharParsing (digit)
-- ficha 2 de programação funcional 
--1 a) 39.0
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

--1 b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
             else (funB t) 

--1 c) 
funC :: [a] -> [a]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

--2 a)
dobros :: [Float] -> [Float]
dobros [] =[]
dobros (h:t) = h*2 : dobros t 

--2 b)
numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre y (x:xs) = if x ==y 
                     then  1 + numOcorre y xs 
                     else 0 * numOcorre y xs 

--2 c)
positivos :: [Int] -> Bool
positivos [] = error "não pinta"
positivos (h:t) = if h<0 then False else positivos t


--2 d)
sopos :: [Int] -> [Int]
sopos [] = []
sopos (h:t) = if h >0 
              then h : sopos t 
              else sopos t

--2 e)
somaNeg :: [Int] -> Int
somaNeg [] = 0 
somaNeg (h:t) = if h< 0 
                then h + somaNeg t
                else somaNeg t 

--2 i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] =(0,0,0)
sumTriplosc((x,y,z):xs) =
    let (a,b,c) = sumTriplos xs 
    in (x+a,y+b,z+c)
--3 a) 
{-soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if ord 'a' <= ord h && ord 'z' >= ord h || ord 'A' <= ord h && ord 'Z' >= ord h
                  then h:soDigitos t 
                  else soDigitos t-}

soDigitos2 :: [Char] -> [Char]
soDigitos2 [] = []
sodigitos2 (h:t) | isDigit h = h : sodigitos2 t 
                 |otherwise = sodigitos2 t 


--3 b)
minusculas :: [Char] -> Int 
minusculas [] = 0
minusculas (h:t) = if ord 'a' <= ord h && ord 'z' >= ord h 
                   then 1 + minusculas t
                   else minusculas t 


--3 c)
nums :: String -> [Int]
nums "" = []
nums (h:t) | isDigit h =digitToInt h : nums t 
           | otherwise = nums t

--4. Uma forma de representar polin´omios de uma vari´avel ´e usar listas de mon´omios representados por pares (coeficiente, expoente)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--4 a 
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,y):t) 
 | y == n = 1 + conta n t 
 |otherwise = conta n t


 -- b)
grau :: Polinomio -> Int
grau [] = 0
grau ((x,y): t) 
 | y >= grau t = y
 | otherwise = grau t 
 
-- c) 
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((x,y):xs) 
 | g == y = (x,y) : selgrau g xs
 | otherwise = selgrau g xs

-- d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):xs) 
 | y == 0 = (0,0): deriv xs
 | otherwise = (x*fromIntegral y, y-1) : deriv xs

 -- e)
calcula' :: Float -> Polinomio -> Float 
calcula' _ [] = 0
calcula' z ((x,y):t) = x*(z)^y + calcula' z t

-- f)
simp :: Polinomio -> Polinomio 
simp [] = []
simp ((x,y):t)
 | y == 0 = simp t
 | otherwise = (x,y) : simp t

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((a,b):t) = (x*a,y+b) : mult (x,y) t 

--h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x,y):t) = normalizaAux (x,y) (normaliza t)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux (a,b) [] = []
normalizaAux (a,b) ((x,y):t) 
 | b == y = (x + a, y) : t
 |otherwise = (x,y) : normalizaAux (a,b) t

{-}  let (a,b) = normaliza t
  in if y == b 
      then (x+a, y) : normaliza t
       else (x,y) : normaliza t -}

 --j)

soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma ((c,g):t) p = somaAux (c,g) (soma t p) 

somaAux :: Monomio -> Polinomio -> Polinomio
somaAux m [] = [m]
somaAux (cm,gm) ((c,g):t)
    | gm == g = (cm + c,g) : t
    | otherwise = (c,g) : somaAux (cm,gm) t


-- h) 
produto :: Polinomio -> Polinomio -> Polinomio
produto  [] _ = []
produto (n:t) p = mult n p ++ produto t p 

--k) 
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:t) = insere' m (ordena t)

insere' :: Monomio -> Polinomio -> Polinomio
insere' (cm,gm) [] = [(cm,gm)]
insere' (cm,gm) ((c,g):t)
    | g > gm = (cm,gm) : (c,g) : t
    | otherwise = (c,g) : insere' (cm,gm) t

--l)
equiv :: Polinomio -> Polinomio -> Bool
equiv  [] _ = False 
equiv _ [] = False 
equiv p1 p2 =  if ordena ( normaliza p1) == ordena ( normaliza p2 ) then True else False 