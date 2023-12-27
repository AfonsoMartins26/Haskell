module TestePf.Fichapf9 where
import System.Random
import Data.List
import Data.Maybe
import Data.Char (digitToInt)

--1
--(a) 

bingo :: IO () 
bingo = bingoaux []

bingoaux :: [Int] -> IO()
bingoaux l 
  | length l == 90 = return ()
  |otherwise = do numerogerado l 

numerogerado :: [Int] -> IO()
numerogerado l =  do  
                  n <- randomRIO(1,90)
                  if n `elem` l 
                  then bingoaux l 
                  else bingoaux (n:l)  

--(b)
mastermind :: IO ()
mastermind = do ss <- geraSeqSecreta 
                jogar ss 


jogar :: (Int,Int,Int,Int) -> IO()
jogar cs = do ns <- lerNumeros 
              if cs == ns 
              then print "Acertou!"
              else let (i,_) = feedback cs ns 
                   in do print ("Iguais: " ++ show i)  
                         jogar cs 

geraSeqSecreta :: IO(Int,Int,Int,Int) 
geraSeqSecreta = do c1 <- randomRIO (0,9)
                    c2 <- randomRIO (0,9)
                    c3 <- randomRIO (0,9)
                    c4 <- randomRIO (0,9) 
                    return (c1,c2,c3,c4)


lerNumeros :: IO (Int,Int,Int,Int) 
lerNumeros = do print "Introduza 4 digitos:"
                d1 <- getChar
                d2 <- getChar 
                d3 <- getChar 
                d4 <- getChar 
                getChar
                return (digitToInt d1 
                        ,digitToInt d2 
                        ,digitToInt d3 
                        ,digitToInt d4)


feedback :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int,Int) 
feedback (c1,c2,c3,c4) (d1,d2,d3,d4) =  let iguais = length (filter (==True )[c1==d1,c2==d2,c3==d3,c4==d4])
                                        in (iguais,0)

maxiumMB :: Ord a => [Maybe a] -> Maybe a 
maxiumMB [] = Nothing 
maxiumMB (Nothing:xs) = maxiumMB xs 
maxiumMB (Just x:xs) = case maxiumMB xs of 
    Nothing -> Just x 
    Just y -> Just (max x y) 

--2
data Aposta = Ap [Int] (Int,Int)

--(a)
valida :: Aposta -> Bool 
valida (Ap l (e1,e2)) = validaintervalo l [1..50] && e1 `elem` [1..9] && e1 `elem` [1..9] && length l == 5 && length (nub l)== 5 && (e1 /= e2) 
-- nub l serve caso l tenha numeros repetidos   

validaintervalo :: [Int] -> [Int] -> Bool 
validaintervalo [] _ = False 
validaintervalo (h:t) intervalo 
    | h `elem` intervalo = validaintervalo t intervalo 
    |otherwise = False 

--(b) 
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap lista1 (x1,y1)) (Ap lista2 (x2,y2)) = (comunsaux lista1 lista2, comunsaux [x1,y1] [x2,y2]) 

comunsaux :: [Int] -> [Int] -> Int
comunsaux (x:xs) (y:ys) 
   |x == y = 1 + comunsaux xs ys 
   |otherwise = comunsaux xs ys 

--(c)
--i
instance Eq Aposta where 
   (==) :: Aposta -> Aposta -> Bool
   (==) x y =comuns x y == (5,2) 

--ii
premio :: Aposta -> Aposta -> Maybe Int
premio aposta1 aposta2
         | comuns aposta1 aposta2 == (5,2) = Just 1 
         | comuns aposta1 aposta2 == (5,1) = Just 2 
         | comuns aposta1 aposta2 == (5,0) = Just 3 
         | comuns aposta1 aposta2 == (4,2) = Just 4 
         | comuns aposta1 aposta2 == (4,1) = Just 5 
         | comuns aposta1 aposta2 == (4,0) = Just 6 
         | comuns aposta1 aposta2 == (3,2) = Just 7 
         | comuns aposta1 aposta2 == (2,2) = Just 8 
         | comuns aposta1 aposta2 == (3,1) = Just 9 
         | comuns aposta1 aposta2 == (3,0) = Just 10 
         | comuns aposta1 aposta2 == (1,2) = Just 11 
         | comuns aposta1 aposta2 == (2,1) = Just 12
         | comuns aposta1 aposta2 == (2,0) = Just 13
         |otherwise = Nothing
          
-- ou 

premio' :: Aposta -> Aposta -> Maybe Int
premio' x y =
    case (comuns x y) of
        (5,n) -> Just (3 - n)
        (4,n) -> Just (6 - n)
        (3,n) -> Just (10 - n - (if n == 2 then 1 else 0))
        (2,2) -> Just 8 
        (1,2) -> Just 11
        (2,n) -> Just (13 - n)
        _ -> Nothing


--(d)
--i
leAposta :: IO Aposta
leAposta = do
    putStr $ "Introduza os numeros: "
    nums <- getLine
    let listaNums = map read (words nums) :: [Int]
    putStr $ "Introduza as estrelas: "
    estrelas <- getLine
    let parEstrelas = map read (words estrelas) :: [Int]
    let parEstrelas' = (parEstrelas !! 0, parEstrelas !! 1)
    let aposta = (Ap listaNums parEstrelas')
    if valida aposta then return aposta else do
        print $ "Aposta invalida"
        leAposta 

--ii 
joga :: Aposta -> IO ()
joga chave = do
    aposta <- leAposta
    let flag = premio chave aposta
    if isJust $ flag then print $ fromJust $ premio chave aposta else print $ "Nao ganhou nada"

--(e) 
geraChave :: IO Aposta
geraChave = do 
   n1 <- randomRIO (1,50)
   n2 <- randomRIO (1,50)
   n3 <- randomRIO (1,50)
   n4 <- randomRIO (1,50)
   n5 <- randomRIO (1,50)
   e1 <- randomRIO (1,9)
   e2 <- randomRIO (1,9)
   let l = [n1,n2,n3,n4,n5]
   if (length ( nub l)) == length l && (e1/= e2) then return ( Ap l (e1,e2)) else geraChave 

--(f)
main :: IO ()
main = do 
        ch <- geraChave
        ciclo ch
menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
      where menutxt = unlines ["",
                               "Apostar ........... 1",
                               "Gerar nova chave .. 2",
                                "",
                                "Sair .............. 0"]
ciclo :: Aposta -> IO ()
ciclo chave = do 
  opcao<-menu 
  case opcao of 
    "1" -> do 
      joga chave
      ciclo chave 
    "2" -> do 
      print $ "nova chave"
      main 
    "0" -> return() 
    _ -> ciclo chave 

