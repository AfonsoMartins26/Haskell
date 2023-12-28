module TestePf.Teste2020 where
import Data.List (groupBy)
-- teste 11 de janeiro de 2020

--1 
-- (a) 
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] l = [] 
intersect l [] = [] 
intersect (h1:t1) (h2:t2) 
   |h1 == h2 = h1: intersect t1 (h2:t2) 
   |otherwise = intersect t1 (h2:t2) 

-- (b)
tails :: [a] -> [[a]]
tails [] = [[]] 
tails (h:t) = (h:t) : tails t  

--2 
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

-- (a) 
elems :: ConjInt -> [Int]
elems [] = [] 
elems ((h,h'):t) = [h..h'] ++ elems t 

-- (b)  dificil 
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj (h:t) =
    case geraconj t of 
        [] -> [(h,h)]
        (a,b) : r 
            | a == succ h -> (h,b) : r     -- succ h numero depois de h ou h+1
            | otherwise -> (h,h) : (a,b) :  r

-- 3 
data Contacto = Casa Integer
    | Trab Integer
    | Tlm Integer
    | Email String
    deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

 -- (a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail name email [] = [(name ,[Email email])]
acrescEmail name email ((n,c):t) 
     | name == n = ((n,Email email:c)) :t 
     | otherwise = (n,c) : acrescEmail name email t 

-- (b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails name [] = Nothing 
verEmails name ((n,contacto):t) 
     | name == n = Just (getEmails contacto) 
     | otherwise = verEmails name t 


getEmails :: [Contacto] -> [String]
getEmails [] = [] 
getEmails (Email e:t) = e : getEmails t  
getEmails (_:t) = getEmails t 


-- (c) 
consulta :: [Contacto] -> ([Integer],[String]) 
consulta  [] = ([],[])
consulta l =(retiratele l,getEmails l  ) 


retiratele :: [Contacto] -> [Integer] 
retiratele [] = []  
retiratele (Casa n : t) = n : retiratele t  
retiratele (Trab n : t) = n : retiratele t 
retiratele (Tlm n : t) = n : retiratele t
retiratele (_ : t) = retiratele t


-- (d)
consultaIO :: Agenda -> IO ()
consultaIO  lista = do 
           putStr $ "Insira um  nome : " -- escreve a string 
           name <- getLine 
           let contactos = dacontactos name lista
           putStrLn("Contatos de " ++ name ++ ": " ++ show contactos) -- putStrLn escreve a string e depois muda de linha 


dacontactos :: Nome -> Agenda -> [Contacto] 
dacontactos name [] = [] 
dacontactos name ((n,c):t) 
   | name == n = c ++ dacontactos name t 
   |otherwise = dacontactos name t  


-- 4 
data RTree a = R a [RTree a] deriving (Show, Eq)

-- (a) 
paths :: RTree a -> [[a]]
paths (R x []) = [[x]] 
paths (R x  t) = map (x:) $ concatMap paths t  -- o concatMap aplica uma funcao creadroa de listas numa lista 

-- (b) não entendi ... (solucao da sofia)
unpaths :: Eq a => [[a]] -> RTree a
unpaths [[x]] = R x []
unpaths l = R root (map unpaths $ groupBy (\a b -> head a == head b) $ map tail l)
    where root = head $ head l  
