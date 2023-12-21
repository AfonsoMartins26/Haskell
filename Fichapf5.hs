module TestePf.Fichapf5 where

import Data.List


--1
--(a) any :: (a -> Bool) -> [a] -> Bool que teste se um predicado ´e verdade para  algum elemento de uma lista; por exemplo:
any' ::  (a -> Bool) -> [a] -> Bool
any' f [] = False 
any' f (h:t) = f h || any' f t 

{-(b) zipWith :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de
duas listas usando uma fun¸c˜ao espec´ıfica; por exemplo:
zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44].-}

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] []= []
zipWith' f (h:t)(h1:t1)= f h h1 :zipWith'  f t t1 

{-(c) takeWhile :: (a->Bool) -> [a] -> [a] que determina os primeiros elementos
da lista que satisfazem um dado predicado; por exemplo:
takeWhile odd [1,3,4,5,6,6] == [1,3].-}

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = [] 
takeWhile' f (h:t) 
     |f h= h : takeWhile' f t 
     |otherwise = [] 

{-(d) dropWhile :: (a->Bool) -> [a] -> [a] que elimina os primeiros elementos da
lista que satisfazem um dado predicado; por exemplo:
dropWhile odd [1,3,4,5,6,6] == [4,5,6,6].-} 
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) 
     | f h = dropWhile' f t 
     |otherwise = (h:t)

{-(e) span :: (a-> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os dois
resultados anteriores. Note que apesar de poder ser definida `a custa das outras
duas, usando a defini¸c˜ao-}
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t)
  |f h =let (take,drop) = span f t in (h:take,drop)
  |otherwise = ([],(h:t))

{-(f) deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] que apaga o primeiro elemento de uma lista que ´e “igual” a um dado elemento de acordo com a fun¸c˜ao
de compara¸c˜ao que ´e passada como parˆametro. Por exemplo:
deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)] -}
deleteBy' :: (a->a->Bool)->a->[a]->[a]
deleteBy' f  x (h:t)
    | f x h = t
    |otherwise = h: deleteBy' f x t 

{-(g) sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma lista comparando os resultados
 de aplicar uma fun¸c˜ao de extrac¸c˜ao de uma chave a cada elemento de uma lista. Por exemplo:
sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)].-}

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = [] 
sortOn' f ( h:t) = insertOn' f h (sortOn' f t) 

insertOn' ::  (Ord b) => (a -> b) -> a -> [a] -> [a]
insertOn' _ x [] = [x]
insertOn' f x (h:t) = if f x > f h 
                      then h : insertOn' f x t 
                      else x : h : t

--2 
type Polinomio = [Monomio]
type Monomio = (Float,Int)

{-(a) selgrau :: Int -> Polinomio -> Polinomio que selecciona os mon´omios com
um dado grau de um polin´omio.-}

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\x -> snd x == n )p 

{-(b) conta :: Int -> Polinomio -> Int de 
forma a que (conta n p) indica quantos mon´omios de grau n existem em p.-}

conta :: Int -> Polinomio -> Int 
conta n p = length $ selgrau n p 

{-(c) grau :: Polinomio -> Int que indica o grau de um polin´omio.-}
grau :: Polinomio -> Int
grau p = maximum $ map snd p 

{-(d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polin´omio.-}
deriv :: Polinomio -> Polinomio 
deriv p = map (\(c,g) -> (c * fromIntegral g, g - 1)) $ filter (\(c,g) -> g /= 0) p

{-(e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polin´omio
para uma dado valor de x.-}

calcula :: Float -> Polinomio -> Float 
calcula x p = foldl (\acc (c,g) -> acc + c * x ^ g) 0 p 

{-(f) simp :: Polinomio -> Polinomio que retira de um polin´omio os mon´omios de
coeficiente zero.-}
simp :: Polinomio -> Polinomio 
simp  = filter (\(c,g) -> c /= 0)

{-(g) mult :: Monomio -> Polinomio -> Polinomio que calcula 
o resultado da multiplica¸c˜ao de um mon´omio por um polin´omio.-}
mult :: Monomio -> Polinomio -> Polinomio
mult (cm,gm) = map (\(c,g) -> (cm*c,gm+g))

{-(h) ordena :: Polinomio -> Polinomio que ordena 
um polon´omio por ordem crescente dos graus dos seus mon´omios-}
ordena :: Polinomio -> Polinomio
ordena = sortBy (\(grau1, _) (grau2, _) -> compare grau1 grau2)

{-(i) normaliza :: Polinomio -> Polinomio que dado um polin´omio constr´oi um
polin´omio equivalente em que n˜ao podem aparecer varios mon´omios com o mesmo
grau-}
normaliza :: Polinomio -> Polinomio
normaliza p = foldl (\acc m -> adiciona m acc) [] p
    where adiciona :: Monomio -> Polinomio -> Polinomio
          adiciona m [] = [m]
          adiciona (cm,gm) ((c,g):t) = if gm == g then (cm+c,g) : t else (c,g) : adiciona (cm,gm) t

{-(j) soma :: Polinomio -> Polinomio -> Polinomio que faz a soma de dois polin´omios
de forma que se os polin´omios que recebe estiverem normalizados produz tamb´em
um polin´omio normalizado.-}
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = foldl (\acc m -> adiciona m acc) p1 p2
    where adiciona :: Monomio -> Polinomio -> Polinomio
          adiciona m [] = [m]
          adiciona (cm,gm) ((c,g):t) = if gm == g then (cm+c,g) : t else (c,g) : adiciona (cm,gm) t


{-(k) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de
dois polin´omios-}
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\acc m -> soma (mult m p2) acc) [] p1

{-(l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polin´omios s˜ao
equivalentes-}
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1 ) == ordena (normaliza p2) 


-- 3 
type Mat a = [[a]]

{-(a) dimOK :: Mat a -> Bool que testa se uma matriz est´a bem constru´ıda (i.e., se
todas as linhas tˆem a mesma dimens˜ao).-}
dimOK :: Mat a -> Bool
dimOK = (== 1) . length . nub . map length

{-(b) dimMat :: Mat a -> (Int,Int) que calcula a dimens˜ao de uma matriz.-}

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m = (length m,length (head m) )

{-(c) addMat :: Num a => Mat a -> Mat a -> Mat a que adiciona duas matrizes.-}

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat  =zipWith' (zipWith' (+) ) 

{-(d) transpose :: Mat a -> Mat a que calcula a transposta de uma matriz.-}
transpose :: Mat a -> Mat a
transpose m = [ map (!! i) m |  i <- [0..c-1]]
    where (l,c) = dimMat m 


{-(e) multMat :: Num a => Mat a -> Mat a -> Mat a que calcula o produto de duas
matrizes.-}
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = map (\l -> [ sum (zipWith (*) l [ x !! i | x <- m2 ]) | i <- [0..c-1] ]) m1
    where (l,_) = dimMat m1
          (_,c) = dimMat m2 


{-(f) zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c que, à semelhança do
que acontece com a função zipWith, combina duas matrizes. Use essa função para 
definir uma função que adiciona duas matrizes.-}

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat = zipWith . zipWith

addMat' :: Num a => Mat a -> Mat a -> Mat a
addMat' = zipWMat (+)


{-(g) triSup :: (Num a, Eq a) => Mat a -> Bool que testa se uma matriz quadrada é triangular superior
 (i.e., todos os elementos abaixo da diagonal são nulos).-}


triSup :: (Num a, Eq a) => Mat a -> Bool
triSup m = and [ all (== 0) [ m !! i !! j | j <- [0..i-1] ] | i <- [0..length m - 1]]


{-(h) rotateLeft :: Mat a -> Mat a que roda uma matriz 90º para a esquerda. 
Por exemplo, o resultado de rodar a matriz acima apresentada deve corresponder à matriz:
| 3 5 6 |
| 2 4 0 |
| 1 0 0 |
-}

rotateLeft :: Mat a -> Mat a
rotateLeft m = [ map (!! i) m | i <- [c-1,c-2..0]] 
    where (l,c) = dimMat m