module TestePf.Fichapf2 where
import TestePf.Fichapf1 


--1 
--(a) Considere a seguinte defini¸c˜ao:
funA :: [Double] -> Double
funA ys = foldr (\ y -> (+) (y ^ 2)) 0 ys

--  funA [2,3,5,1] = 39.0 

-- (b) Considere seguinte defini¸c˜ao:
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if even h then h : funB t
                             else funB t
-- funB [8,5,12] =[8,12]

--(c) Considere a seguinte defini¸c˜ao:
funC :: [a] -> [a]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []
-- funC [1,2,3,4,5] = [5]

--(d) Considere a seguinte defini¸c˜ao:
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

--funD "otrec" = "certo" 

--2 
--(a) dobros :: [Float] -> [Float] que recebe uma lista e produz a lista em que cada elemento ´e o dobro do valor correspondente na lista de entrada.
dobros :: [Float] -> [Float]
dobros t = map (* 2) t

--(b) numOcorre :: Char -> String -> Int que calcula o n´umero de vezes que um caracter ocorre numa string.
numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre a (h:t)
    |a ==h =1 + numOcorre a t
    |otherwise= numOcorre a t

--(c) positivos :: [Int] -> Bool que testa se uma lista s´o tem elementos positivos.
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = not (h <= 0) && positivos t

--(d) soPos :: [Int] -> [Int] que retira todos os elementos n˜ao positivos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)
     | h <= 0 = soPos t
     | otherwise = h : soPos t

-- (e) somaNeg :: [Int] -> Int que soma todos os n´umeros negativos da lista de entrada.
somaNeg :: [Int] -> Int
somaNeg  [] = 0
somaNeg (h:t)
     | h < 0 = h + somaNeg t
     |otherwise = somaNeg t

--(f) tresUlt :: [a] -> [a] devolve os ´ultimos trˆes elementos de uma lista. Se a lista de entrada tiver menos de trˆes elementos, devolve a pr´opria lista.
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t)
      | length t < 3 = h:t
      |otherwise = tresUlt t

--(g) segundos :: [(a,b)] -> [b] que calcula a lista das segundas componentes dos pares.
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (h:t) = snd h  : segundos t

--(h) nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool que testa se um elemento aparece na lista como primeira componente de algum dos pares.
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros n ((a,b):t)
  | n == a = True
  | otherwise = nosPrimeiros n t 

{- (i) sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) soma uma
lista de triplos componente a componente.
Por exemplo, sumTriplos [(2,4,11), (3,1,-5), (10,-3,6)] = (15,2,12)-}

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0) 
sumTriplos ((q,w,e):t) = (q+tq,w+tw,e+te)
                         where (tq,tw,te) = sumTriplos t 
--3
--(a) soDigitos :: [Char] -> [Char] que recebe uma lista de caracteres, e selecciona dessa lista os caracteres que s˜ao algarismos.
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)
    | isDigit h = h : soDigitos t
    | otherwise = soDigitos t

--(b) minusculas :: [Char] -> Int que recebe uma lista de caracteres, e conta quantos desses caracteres s˜ao letras min´usculas.
minusculas :: [Char] -> Int
minusculas [] =0 
minusculas (h:t) 
   | isLower' h == True = 1 + minusculas t 
   | otherwise = minusculas t 

-- (c) nums :: String -> [Int] que recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string, pela mesma ordem.

nums :: String -> [Int] 
nums "" = [] 
nums (h:t) 
  |isDigit h == True = digitToInt h: nums t 
  |otherwise = nums t 

--4 
type Polinomio = [Monomio]
type Monomio = (Float,Int)
 
--(a) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quantos mon´omios de grau n existem em p
conta :: Int -> Polinomio -> Int 
conta _ [] = 0 
conta n ((q,e):t) 
     | n == e = 1 + conta n t 
     | otherwise = conta n t 

-- (b) grau :: Polinomio -> Int que indica o grau de um polin´omio.
grau :: Polinomio -> Int 
grau [] = 0 
grau ((q,e):t)
   | e > grau t = e 
   |otherwise = grau t  

--(c) selgrau :: Int -> Polinomio -> Polinomio que selecciona os mon´omios com um dado grau de um polin´omio.
selgrau :: Int -> Polinomio -> Polinomio 
selgrau _ [] = [] 
selgrau n ((q,e):t) 
   |n == e = (q,e): selgrau n t 
   | otherwise = selgrau n t 

-- (d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polin´omio. 
deriv :: Polinomio -> Polinomio 
deriv [] = [] 
deriv ((q,e):t) 
      |e == 0 = deriv t 
      |otherwise = ((q*fromIntegral e),e-1): deriv t 

-- (e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polin´omio para uma dado valor de x.
calcula :: Float -> Polinomio -> Float 
calcula _ [] = 0 
calcula m ((q,e):t) = q*(m^2) + calcula m t 

-- (f) simp :: Polinomio -> Polinomio que retira de um polin´omio os mon´omios de coeficiente zero.
simp :: Polinomio -> Polinomio 
simp [] = [] 
simp ((q,e):t) 
     | q ==0 = simp t 
     | otherwise = (q,e):simp t 
 
--(g) mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da multiplica¸c˜ao de um mon´omio por um polin´omio.
mult :: Monomio -> Polinomio -> Polinomio 
mult _ [] = []
mult (q,e) ((r,y):t) = (q*r,e+y ) : mult (q,e) t 

{-(h) normaliza :: Polinomio -> Polinomio que dado um polin´omio constr´oi um
polin´omio equivalente em que n˜ao podem aparecer varios mon´omios com o mesmo
grau. -}

normaliza :: Polinomio -> Polinomio
normaliza [] =[]
normaliza ((q,e):t)= normalizaaux (q,e) (normaliza t) 


normalizaaux :: Monomio -> Polinomio -> Polinomio 
normalizaaux m [] = [m]
normalizaaux (q,e)((r,y):t)
      | e == y = ((r+q,y):t)
      |otherwise = (r,y): normalizaaux (q,e) t 

{-(i) soma :: Polinomio -> Polinomio -> Polinomio que soma dois polin´omios de
forma a que se os polin´omios que recebe estiverem normalizados produz tamb´em
um polin´omio normalizado-}
soma :: Polinomio -> Polinomio -> Polinomio
soma [] p = p 
soma p [] = p 
soma  ((q,e):t) p = normalizaaux (q,e) (soma t p) 

--(j) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de dois polin´omios
produto :: Polinomio -> Polinomio -> Polinomio
produto p [] = []
produto [] p = []
produto (m:t) p = (mult m p) ++ produto t p 


--(k) ordena :: Polinomio -> Polinomio que ordena um polin´omio por ordem crescente dos graus dos seus mon´omios.
ordena :: Polinomio -> Polinomio 
ordena []=[] 
ordena ((q,e):t) = insere (q,e) (ordena t) 


insere :: Monomio -> Polinomio -> Polinomio 
insere x [] = [x] 
insere (q,e)((r,y):t) 
      | e< y = (q,e): ((r,y):t)
      |otherwise = (r,y): insere (q,e) t 

--(l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polin´omios s˜ao equivalentes
equiv :: Polinomio -> Polinomio -> Bool 
equiv  p1 p2 = ordena (normaliza p1 ) == ordena (normaliza p2) 


 