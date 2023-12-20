module TestePf.Fichapf4 where
import Data.Char 

 
{-1. Defina a fun¸c˜ao digitAlpha :: String -> (String,String), que dada uma string,
devolve um par de strings: uma apenas com as letras presentes nessa string, e a outra
apenas com os n´umeros presentes na string. Implemente a fun¸c˜ao de modo a fazer uma
´unica travessia da string. Relembre que as fun¸c˜oes isDigit,isAlpha :: Char -> Bool
est˜ao j´a definidas no m´odulo Data.Char.-}

digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t) 
     | isDigit h =  (h:digits,letters)
     |isAlpha h =  (digits,h:letters)
     |otherwise = (digits,letters)
          where (digits,letters) = digitAlpha t 

{-2. Defina a fun¸c˜ao nzp :: [Int] -> (Int,Int,Int) que, dada uma lista de inteiros,
conta o n´umero de valores nagativos, o n´umero de zeros e o n´umero de valores positivos,
devolvendo um triplo com essa informa¸c˜ao. Certifique-se que a fun¸c˜ao que definiu
percorre a lista apenas uma vez.-} 
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) 
   | h <0 = (n + 1,z,p)
   |h == 0 = (n,z+1,p)
   |otherwise  = (n,z,p+1)
   where (n,z,p) = nzp t 

{-3. Defina a fun¸c˜ao divMod :: Integral a => a -> a -> (a, a)
 que calcula simultaneamente a divis˜ao e o resto da divis˜ao inteira por subtrac¸c˜oes sucessivas.-}
divMod' :: Integral a => a -> a -> (a, a)
divMod' x y 
    | x < 0 = let (q,r) = divMod' (-x) y in (if r == 0 then (-q,r) else (-q-1,-r+y))
    | y < 0 = let (q,r) = divMod' (-x) (-y) in (q,-r)
    | otherwise = if x - y < 0 then (0,x) else (let (q,r) = divMod' (x - y) y in (q+1,r))

{-4. Utilizando uma fun¸c˜ao auxiliar com um acumulador, optimize seguinte 
defini¸c˜ao recursiva que determina qual o n´umero que corresponde a uma lista de digitos.-}
fromDigits' :: [Int] -> Int 
fromDigits' l = fromDigitsaux l 0 

fromDigitsaux :: [Int] -> Int -> Int 
fromDigitsaux [] acc = acc 
fromDigitsaux (h:t) acc = fromDigitsaux t (h+10* acc )

{-5. Utilizando uma fun¸c˜ao auxiliar com acumuladores, optimize a seguinte defini¸c˜ao que
determina a soma do segmento inicial de uma lista com soma m´axima.-}
inits' :: [a] -> [[a]]
inits' [] =[[]]
inits' l = inits' (init l) ++ [l]

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits' l] 

maxSuminit' :: (Num a, Ord a) => [a] -> a
maxSuminit' l = maxSuminitaux l (sum l)

maxSuminitaux :: (Num a, Ord a) => [a] -> a -> a
maxSuminitaux [] acc = acc 
maxSuminitaux l acc = if si > acc 
                      then maxSuminitaux il si 
                      else maxSuminitaux il acc 
                          where il = init l 
                                si = sum il 
{-6. Optimize a seguinte defini¸c˜ao recursiva da fun¸c˜ao que calcula o 
n-´esimo n´umero da sequˆencia de Fibonacci, usando uma fun¸c˜ao auxiliar com 2 
acumuladores que representam, respectivamente, o n-´esimo e o n+1-´esimo n´umeros dessa sequˆencia.-}

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib' (n-2) 0 1 

fib' :: Int -> Int -> Int -> Int 
fib' 0 n _ = n 
fib' i fib_n fib_n_mais_1 = fib' (i-1) fib_n_mais_1 (fib_n +fib_n_mais_1 ) -- fib' 2 0 1 = 1 pois 
                                                                         -- fib' (2-1) 1 (1+0)
                                                                         -- fib' 1 1 1 
                                                                         -- fib' 0 1 1 = 1 

{-7. Defina a fun¸c˜ao intToStr :: Integer -> String que converte um inteiro numa
string. Utilize uma fun¸c˜ao auxiliar com um acumulador onde vai construindo a string
que vai devolver no final.-}

intToStr :: Integer -> String 
intToStr 0 = "zero"
intToStr n = intToStraux n ""

intToStraux :: Integer -> String -> String 
intToStraux 0 ('-':acc) = acc 
intToStraux n acc = intToStraux nn ((case r of 
        0 -> "-zero"
        1 -> "-um"
        2 -> "-dois"
        3 -> "-três"
        4 -> "-quatro"
        5 -> "-cinco"
        6 -> "-seis"
        7 -> "-sete"
        8 -> "-oito"
        9 -> "-nove")++ acc )
     where (nn,r) = n `divMod` 10 

{-8) Para cada uma das expressões seguintes, exprima por enumeração a lista correspondente.
 Tente ainda, para cada caso, descobrir uma outra forma de obter o mesmo resultado.-}
--(a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]

-- R : [x | x <- [1..20], mod x 6 == 0 ]

--(b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]

-- R : [x | x <- [1..20], mod x 6 == 0]

--(c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]

-- R : [(x,30-x) | x <- [10..20]]

--(d) [sum [y | y <- [1..x], odd y] | x <- [1..10]]

-- R : [ x^2 | x <- [1..5], y <- [1..2]] -- é quantas vezes se repete o numero se x = 1 vai aparecer dois numeros 1 

{-9. Defina cada uma das listas seguintes por compreens˜ao. -}

--(a) [1,2,4,8,16,32,64,128,256,512,1024]

-- R : [2^x | x <- [0..10]]

--(b) [(1,5),(2,4),(3,3),(4,2),(5,1)]

-- R : [(x,y) | x <- [1..5],y <- [1..5],x+y== 6]

--(c) [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]] 

-- R : [[1..x] | x <-[1..5]]

--(d) [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]

-- R : [ replicate x 1 | x <- [1..5]] -- replica o 1 x vezes 

--(e) [1,2,6,24,120,720]

--R : [ product [y | y <- [1..x]] | x <- [1..6]] -- se x = 2 então o resultado seria 1 * 2 = 2 
                                                 -- se x = 3 o resultado seria 1* 2*3 =6 
                                                 -- e assim por adiante (1*2*3*4 = 24 )    
