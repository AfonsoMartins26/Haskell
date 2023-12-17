module Ficha9 where
import System.Random 
import Data.Char (digitToInt)




dataNasc :: IO (Int,Int,Int)
dataNasc = do d <- randomRIO (1,31)
              m <- randomRIO (1,12)
              a <- randomRIO (1900,2024)
              return (d,m,a) 

{-

type Mat a = [[a]]


gerarMatriz :: (Int,Int)   -- linha , col 
            -> (Int,Int)   -- intervalo de valores 
            -> IO (Mat Int)


gerarMatriz (0,_) _ = return []
gerarMatriz (x,y)(a,b) = do l <- geraLinha y (a,b)
                            ls <- gerarMatriz (x-1,y)(a,b)
                            return (l:ls)


geraLinha :: Int -> (Int,Int) -> IO [Int]
geraLinha 0 _ =  return [] 
geraLinha n (a,b) = do v <- randomRIO (a,b)
                       l <- geraLinha (n-1)(a,b)
                       return (v:l)
-}
--1 (a)
bingo :: IO()
bingo = do l <- geraNumeros []
           print l 


geraNumeros :: [Int] -> IO [Int]
geraNumeros l |length l == 90 = return l 
              |otherwise     = do n <- randomRIO (1,90)
                                  print n 
                                  getChar 
                                  if n `elem` l 
                                  then geraNumeros l 
                                  else geraNumeros (n:l)

-- (b) 

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



{-
stringToMap "2,3,4,6\n12,4,7,8\n1,99,-5,88"
-}
type Mat = [[Int]]


stringToMat :: String -> Mat 
stringToMat s = map stringToVector (lines s)

stringToVector :: String -> [Int] 
stringToVector l =  read ("[" ++ l ++ "]") 

data BTree a = Empty | Node a (BTree a) (BTree a)

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = (inorder e) ++ (r:inorder d)



numera :: BTree a -> BTree (a,Int)
numera t =  fst $ numera' t 1


numera' :: BTree a -> Int -> (BTree (a,Int),Int) 
numera' Empty  n    = (Empty ,n)
numera' (Node i e d) n = (Node (i,n)e' d',nd)   
       where  (e',ne )     = numera' e (n+1) 
              (d',nd)     = numera' d ne


