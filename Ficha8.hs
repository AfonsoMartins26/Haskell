module Ficha8 where


data Ordering = LT | EQ | GT
data Frac = F Integer Integer




-- (a) Defina a fun¸c˜ao normaliza :: Frac -> Frac, que dada uma frac¸c˜ao calcula uma frac¸c˜ao equivalente, irredut´ıvel, e com o denominador positivo.

mdc :: Integer -> Integer -> Integer
mdc x 0 = x 
mdc 0 y = y 
mdc x y = mdc y (x `mod` y) 

normaliza :: Frac -> Frac
normaliza  (F x y)  
    | y < 0 = normaliza $ F (-x) (-y) 
    | otherwise  = 
        let d = mdc x y in 
            F (x `div` d) (y `div` d)

--(b) Defina Frac como instˆancia da classe Eq.
  
instance Eq Frac where 
    (==) :: Frac  -> Frac  -> Bool 
    f1 == f2 = a1 == a2 && b1 == b2 
        where F a1 b1 = normaliza f1 
              F a2 b2 = normaliza f2 


-- (c) Defina Frac como instˆancia da classe Ord.
 
instance Ord Frac where 
    (<=) :: Frac -> Frac -> Bool
    f1 <= f2 = a1 <= a2 && b1 <= b2 
      where F a1 b1 = normaliza f1 
            F a2 b2 = normaliza f2 


--(d) Defina Frac como instˆancia da classe Show, de forma a que cada frac¸c˜ao seja apresentada por (numerador/denominador).

instance Show Frac where
    show :: Frac -> String 
    show f = show a ++ show b 
        where F a b = normaliza f 



--(e) Defina Frac como instˆancia da classe Num.

{- class (Eq a, Show a) => Num a where
(+), (*), (-) :: a -> a -> a
negate, abs, signum :: a -> a
fromInteger :: Integer -> a -}


instance Num Frac where 
    (+) :: Frac -> Frac ->Frac 
    (F a b) + (F c d) = normaliza $ F (a * d + b * c) (b * d)

    (-) :: Frac -> Frac -> Frac 
    x -y = x + negate y 

    (*) ::Frac -> Frac -> Frac 
    (F a b) * (F c d) = normaliza $ F (a*c) (b*d)

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


--(f) Defina uma fun¸c˜ao que, dada uma frac¸c˜ao f e uma lista de frac¸c˜oes l, selecciona de l os elementos que s˜ao maiores do que o dobro de f.

maiordobro :: Frac -> [Frac] -> [Frac]
maiordobro f l  = filter (> 2*f) l  


--2 


data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)


--(a) Declare Exp a como uma instˆancia de Show

instance Show a => Show (Exp a) where 
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
 -- duas expressoes são iguais quando o  seu valor for igual 

calcula :: Num a => Exp a -> a 
calcula (Const x )= x
calcula (Simetrico x) = - (calcula x) 
calcula (Mais x y) = calcula x + calcula y 
calcula (Menos x y) = calcula x - calcula y 
calcula (Mult x y) = calcula x * calcula y 

--(b)

iguais :: (Num a,Eq a) => Exp a -> Exp a -> Bool 
iguais e1 e2 = calcula e1 == calcula e2

instance (Num a,Eq a) => Eq(Exp a) where
    (==) = iguais



menorIgual :: (Num a,Ord a)  =>  Exp a -> Exp a -> Bool
menorIgual e1 e2 = calcula e1 <= calcula e2 

instance (Num a ,Ord a) => Ord (Exp a) where
    (<=) = menorIgual   


maisExp :: Num a => Exp a -> Exp a -> Exp a 
maisExp  e1 e2 = Const (calcula e1 + calcula e2 )

menosExp e1 e2 = Const ( calcula e1 - calcula e2 )

multExp e1 e2 = Const (calcula e1 * calcula e2)
menosExp,multExp
     :: Num a => Exp a ->Exp a -> Exp a

absExp :: Num a =>Exp a -> Exp a 
absExp e = Const $ abs $ calcula e


signumExp :: Num a => Exp a -> Exp a
signumExp e = Const $ signum $ calcula e 

--3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int -- D Dia mes ano 
data Extracto = Ext Float [(Data, String, Movimento)]


-- (a) Defina Data como instˆancia da classe Ord.
{-
instance Ord Data where
    compare :: Data -> Data -> Ordering
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
        | otherwise = LT
-}

-- (b) Defina Data como instˆancia da classe Show

instance Show Data where 
    show :: Data -> String
    show (D dia mes ano) = show dia ++ "/"++show mes ++"/" ++ show ano 

-- (c) Defina a fun¸c˜ao ordena :: Extracto -> Extracto, que transforma um extracto de modo a que a lista de movimentos apare¸ca ordenada por ordem crescente de data

ordena :: Extracto -> Extracto
ordena (Ext n l) = Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l)



