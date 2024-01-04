module TestePf.Teste2021 where
import Data.Maybe -- importa o mapMaybe 
--20 de Janeiro de 2021

--1 
remove'  :: Eq a => [a] -> [a] -> [a] 
remove' [] _ = [] 
remove' l [] = l 
remove' l (h:t) = remove' (removeaux h l)t

removeaux :: Eq a => a -> [a] -> [a]
removeaux _ [] = []
removeaux x (h:t) 
    | x == h = t 
    |otherwise = removeaux x t 

-- 2 
type MSet a = [(a,Int)]

-- (a)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet  n [] = [] 
removeMSet n ((h,num):t) 
    | n == h = if num <=1 
               then t 
               else ((h,num-1):t) 
    |otherwise = removeMSet n t 

-- (b)
calcula :: MSet a -> ([a],Int)
calcula [] = ([],0)
calcula ((h,n):t)  = (h:tc, n+nt) 
                    where (tc,nt) = calcula t 

-- 3 
partes :: String -> Char -> [String]
partes "" _ = []
partes s delim = 
    case span (/= delim) s of
        ("",rest) -> partes (tail rest) delim
        (part,"") -> [part]
        (part,rest) -> part : partes (tail rest) delim 

-- 4 
data BTree a = Empty | Node a (BTree a) (BTree a)
a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))
-- uma arvore de procura é quando todos os elementos á esequerda sao menores que a raiz e os da direita maior que a raiz 
--(a)
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node a l r)
    | x > a = Node a l (remove x r)
    | x < a = Node a (remove x l) r
    | otherwise =  
         case (l,r) of 
          (Empty,r) -> r 
          (l,Empty) -> l 
          (l,r) -> let (min, sMin) = minSMin r in Node min l sMin 

minSMin :: BTree a -> (a, BTree a)
minSMin (Node e Empty r) = (e, r)
minSMin (Node e l r) = let (min, sMin) = minSMin l in (min, Node e sMin r) 

-- (b )
instance Show a => Show (BTree a) where 
  show :: Show a => BTree a -> String -- Só poderemos declarar (BTree a) como instância da classe Eq 
  show Empty = "*"                  --se o tipo a for também uma instância da classe Eq
  show (Node e l r) = "(" ++ show l ++ " <-" ++ show e ++ "-> " ++ show r ++ ")"


-- 5 
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (h:t) = insertsortOn f h (sortOn' f t)

insertsortOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertsortOn f x [] =[x]
insertsortOn f x (h:t)
        |f x <= f h = (x:h:t)
        |otherwise = h: insertsortOn f x t  

--6
data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String
fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
      Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ] 

-- (a)
fichs :: FileSystem -> [Nome]
fichs (File n) = [n] 
fichs (Dir _ files) = concatMap fichs files 

-- (b)
dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles (File name ) [] = Just [name]
dirFiles (Dir name files) (h:t) 
    | h == name = 
        let results = mapMaybe (`dirFiles` t) files in -- mapMaybe adiciona os elementos se for just qualquer coisa e retira se for nothing
            if null results then
                Nothing
            else
                Just $ concat results
    | otherwise = Nothing
dirFiles _ _ = Nothing


-- (c)
listaFich :: FileSystem -> IO () 
listaFich fs = do
    putStr "> "
    path <- getLine
    case  dirFiles fs (partes path '/') of 
      Just files -> print files 
      Nothing -> putStrLn "Não é uma diretoria."
