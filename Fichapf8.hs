module TestePf.Fichapf8 where
import Data.Char
--1
data Frac = F Integer Integer

{-(a) Defina a fun¸c˜ao normaliza :: Frac -> Frac, que dada uma frac¸c˜ao calcula uma
frac¸c˜ao equivalente, irredut´ıvel, e com o denominador positivo. Por exemplo,
normaliza (F (-33) (-51)) deve retornar F 11 17 e normaliza (F 50 (-5))
deve retornar F (-10) 1. Sugere-se que comece por definir primeiro a fun¸c˜ao
mdc :: Integer -> Integer -> Integer que calcula o m´aximo divisor comum
entre dois n´umeros, baseada na seguinte propriedade (atribuida a Euclides):
mdc x y == mdc (x+y) y == mdc x (y+x)-}

normaliza :: Frac -> Frac
normaliza (F a b)
    | b < 0 = normaliza  (F (-a) (-b))
    | otherwise = 
        let d = mdc a b in
        F (a `div` d) (b `div` d)

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc 0 y = y
mdc x y = mdc y (x `mod` y)


--(b) Defina Frac como instˆancia da classe Eq.

instance Eq Frac where
  (==) :: Frac -> Frac -> Bool
  f1 == f2 = a1 == a2 && b1 == b2 
       where F a1 b1 = normaliza f1 
             F a2 b2 = normaliza f2

--(c) Defina Frac como instˆancia da classe Ord.

instance Ord Frac where 
  (<=) :: Frac -> Frac -> Bool
  f1<=f2 = a1<= a2 && b1 <= b2 
       where F a1 b1 = normaliza f1 
             F a2 b2 = normaliza f2


--(d) Defina Frac como instˆancia da classe Show, de forma a que cada frac¸c˜ao seja apresentada por (numerador/denominador).

instance Show Frac where 
     show :: Frac -> String 
     show f = show a ++ "/" ++ show b 
          where F a b = normaliza f 

--(e) Defina Frac como instˆancia da classe Num. Relembre que a classe Num tem a seguinte defini¸c˜ao
{-
class Num a where
    (+), (*), (-) :: a -> a -> a
    negate, abs, signum :: a -> a
    fromInteger :: Integer -> a
-}
instance Num Frac where
    (+) :: Frac -> Frac -> Frac
    (F a b) + (F c d) = normaliza (F (a*d+b*c) (b*d))
    
    (-) :: Frac -> Frac -> Frac
    x - y = x + negate y

    (*) :: Frac -> Frac -> Frac
    (F a b) * (F c d) = normaliza $ F (a * c) (b * d)
    
    negate :: Frac -> Frac
    negate (F a b) = normaliza $ F (-a) b
    
    abs :: Frac -> Frac
    abs f = F (abs a) b
        where F a b = normaliza f
    
    signum :: Frac -> Frac
    signum f = F (signum a) 1
        where F a b = normaliza f
    
    fromInteger :: Integer -> Frac
    fromInteger x = F x 1
 
{-(f) Defina uma fun¸c˜ao que, dada uma frac¸c˜ao f e uma lista de frac¸c˜oes l, selecciona
de l os elementos que s˜ao maiores do que o dobro de f.-}

maisodobro :: Frac -> [Frac]-> [Frac]
maisodobro (F a b) [] = [] 
maisodobro (F a b) (h:t)
    | normaliza (F (2*a) b ) < h = (h : maisodobro (F a b) t) 
    |otherwise = maisodobro (F a b) t 


-- 2 

data Exp a = Const a
      | Simetrico (Exp a)
      | Mais (Exp a) (Exp a)
      | Menos (Exp a) (Exp a)
      | Mult (Exp a) (Exp a)


-- (a) Declare Exp a como uma instˆancia de Show.

instance Show a => Show (Exp a) where 
  show (Const a) = show a
  show (Simetrico e) = "(- " ++ show e ++ ")"
  show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

--(b) Declare Exp a como uma instˆancia de Eq

instance (Num a, Eq a) => Eq (Exp a) where
    (==) :: (Num a, Eq a) => Exp a -> Exp a -> Bool
    a == b = valueOf a == valueOf b

valueOf :: Num a => Exp a -> a
valueOf (Const a) = a
valueOf (Simetrico a) = negate ( valueOf a)   -- negate $ valueOf a 
valueOf (Mais a b) = valueOf a + valueOf b
valueOf (Menos a b) = valueOf a - valueOf b
valueOf (Mult a b) = valueOf a * valueOf b


--(c) Declare Exp a como instˆancia da classe Num.

instance (Ord a, Num a) => Num (Exp a) where
  (+) :: Num a => Exp a -> Exp a -> Exp a 
  x +y = Mais x y 

  (-) :: Num a => Exp a -> Exp a -> Exp a 
  x -y = Menos x y 

  (*) :: Num a => Exp a -> Exp a -> Exp a
  x * y = Mult x y

  negate :: Num a => Exp a -> Exp a 
  negate (Simetrico a) = a 
  negate a = Simetrico a 

  fromInteger :: Num a => Integer -> Exp a 
  fromInteger n = Const $ fromIntegral n 

  abs:: Num a => Exp a -> Exp a 
  abs (Const a) = Const (abs a)
  abs (Simetrico a) = abs a 
  abs a = if valueOf a < 0 
          then negate a 
          else a 

  signum :: Num a => Exp a -> Exp a  
  signum a                            
    | valueOf a < 0 = Const (-1)     -- retorna -1  se o numero for negativo
    | valueOf a > 0 = Const 1        -- retorna 1 se o numero for positivo 
    |otherwise = Const 0              -- retorna 0 se o numero for 0


-- 3 
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
         deriving (Eq)
data Extracto = Ext Float [(Data, String, Movimento)]

--(a) Defina Data como instˆancia da classe Ord.

instance Ord Data where
    compare :: Data -> Data -> Ordering
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
        | otherwise = LT  


--(b) Defina Data como instˆancia da classe Show

instance Show (Data) where 
  show :: Data -> String
  show (D dia mes ano) = "["++ show dia ++ "/" ++ show mes ++ "/" ++ show ano ++ "]"

 {-(c) Defina a fun¸c˜ao ordena :: Extracto -> Extracto, que transforma um extracto de modo a que a 
 lista de movimentos apare¸ca ordenada por ordem crescente
de data.-} 

ordena :: Extracto -> Extracto
ordena (Ext x []) = (Ext x [])
ordena (Ext x (h:t)) = (Ext x (menores ++ [h] ++ maiores))
    where menores = [x | x <- t, fst' x < fst' h]
          maiores = [x | x <- t, fst' x >= fst' h]
          fst' (d,_,_) = d

{-(d) Defina Extracto como instˆancia da classe Show, de forma a que a apresenta¸c˜ao do
extracto seja por ordem de data do movimento com o seguinte, e com o seguinte
aspecto
Saldo anterior: 300
---------------------------------------
Data Descricao Credito Debito
---------------------------------------
2010/4/5 DEPOSITO 2000
2010/8/10 COMPRA 37,5
2010/9/1 LEV 60
2011/1/7 JUROS 100
2011/1/22 ANUIDADE 8
---------------------------------------
Saldo actual: 2294,5
-}

instance Show Extracto where
    show :: Extracto -> String
    show ext = "Saldo anterior: " ++ show n ++
               "\n---------------------------------------" ++
               "\nData       Descricao" ++ replicate (desc_max - 9) ' ' ++ "Credito" ++ replicate (cred_max - 7) ' ' ++ "Debito" ++
               "\n---------------------------------------\n" ++
               unlines (map (\(dat,desc,mov) -> 
                    show dat ++ replicate (data_max - length (show dat)) ' ' 
                    ++ map toUpper desc ++ replicate (desc_max - length desc) ' ' 
                    ++ case mov of Credito quant -> show quant ++ replicate (cred_max - length (show quant)) ' '; Debito _ -> replicate cred_max ' '
                    ++ case mov of Debito quant -> show quant; Credito _ -> ""
               ) movs) ++
               "---------------------------------------" ++
               "\nSaldo actual: " ++ show ( ext)
        where (Ext n movs) = ordena ext
              data_max = 11
              desc_max = max (length "Descricao   ") (maximum $ map (\(_,desc,_) -> length desc) movs)
              cred_max = max (length "Credito   ") (maximum $ map (\(_,_,mov) -> case mov of Credito x -> length (show x); _ -> 0) movs)
