module Pf4 where
import Data.Char
import GHC.Generics (Generic(from))


digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t)
    | isAlpha h = (  digits, h:letters)
    | isDigit h = (h:digits,   letters)
    | otherwise = (  digits,   letters)
    where (digits, letters) = digitAlpha t


nzp :: [Int ] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) 
  |h < 0 = (negativos+1,zeros,positivos)
  |h == 0 = (negativos,zeros+ 1,positivos)
  |h > 0 = (negativos,zeros,positivos+1)
    where (negativos,zeros,positivos) = nzp t 

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t
fromDigits' :: [Int] -> Int 
fromDigits' l = fst (fromDigits'' l )
    where 
    fromDigits'' [] = (0,0) 
    fromDigits'' (h:t) = let (r,e) = fromDigits'' t 
                         in (h*10^e +r,e +1)


fromDigitsTbIn  :: [Int] -> Int 
fromDigitsTbIn l = sumLce lce 
  where lce = zip l ( reverse [0..(length l-1)])
        sumLce :: [(Int,Int)] -> Int 
        sumLce [] = 0 
        sumLce ((c,e):t) = c*10^e + sumLce t 



fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' n = snd (fib2 n)

fib2 :: Int -> (Int,Int) 
fib2 0 = (0,0)
fib2 1 = (1,1) 
fib2 n = let (r,va) = fib2 (n-1) 
         in (r+ va,r )

