module TestePf.Fichapf1 where
import Data.Char
 
--1 
--(a) perimetro – que calcula o per´ımetro de uma circunferˆencia, dado o comprimento do seu raio

perimetro :: Double -> Double 
perimetro r = 2 * pi * r 

-- (b) dist – que calcula a distˆancia entre dois pontos no plano Cartesiano. Cada ponto ´e um par de valores do tipo Double.
dist :: (Double,Double) -> (Double,Double) -> Double 
dist (x1,y1) (x2,y2) = sqrt ((x2 - x1)^2 + (y2-y1)^2 )


--(c) primUlt – que recebe uma lista e devolve um par com o primeiro e o ´ultimo elemento dessa lista
primUlt :: [a] -> (a,a) 
primUlt l = (head l ,last l )

-- (d) multiplo – tal que multiplo m n testa se o n´umero inteiro m ´e m´ultiplo de n
multiplo :: Int -> Int -> Bool
multiplo m n = mod n m == 0 

--(e) truncaImpar – que recebe uma lista e, se o comprimento da lista for ´ımpar retiralhe o primeiro elemento, caso contr´ario devolve a pr´opria lista.

truncaImpar  :: [a] -> [a] 
truncaImpar l = if  mod (length l) 2  == 1 
                then tail l 
                else l   

-- (f) max2 – que calcula o maior de dois n´umeros inteiros. 
max2 :: Int -> Int -> Int 
max2 m n = if n > m 
           then n 
           else m 


-- (g) max3 – que calcula o maior de trˆes n´umeros inteiros, usando a fun¸c˜ao max2.
max3 :: Int -> Int -> Int -> Int 
max3 n m s = if n > max2 m s 
             then n 
             else max2 m s 


-- 2 
-- (a) A fun¸c˜ao nRaizes que recebe os (3) coeficientes de um polin´omio de 2o grau e que calcula o n´umero de ra´ızes (reais) desse polin´omio.
nRaizes :: (Double,Double,Double) -> Int 
nRaizes (a,b,c)
    | delta > 0 = 2
    | delta == 0 = 1
    | delta < 0 = 0
    where delta = b ^ 2 - 4 * a * c 

--b) A função raizes que, usando a função anterior, recebe os coeficientes do polinómio e calcula a lista das suas raízes reais. 
raizes :: (Double,Double,Double) -> [Double]
raizes (a,b,c)
    | n == 2 = [(-b + sqrt (b ^ 2 - 4 * a * c))/(2*a),(-b - sqrt (b ^ 2 - 4 * a * c))/(2*a)]
    | n == 1 = [(-b + sqrt (b ^ 2 - 4 * a * c))/(2*a)]
    | n == 0 = []
    where n = nRaizes (a,b,c) 

--3. Vamos representar horas por um par de n´umeros inteiros:
type Hora = (Int,Int)

 -- (a) testar se um par de inteiros representa uma hora do dia v´alida;
testahora :: Hora -> Bool 
testahora (h,m) = if 0 <= h && h <= 23 && 0 <= m && h <= 59  
                  then True 
                  else False 

-- (b) testar se uma hora ´e ou n˜ao depois de outra (compara¸c˜ao);

horaconsecutiva :: Hora -> Hora -> Bool 
horaconsecutiva (h,m) (h1,m1) = if h > h1 || h == h1 && m > m1 
                                then True 
                                else False 

-- (c) converter um valor em horas (par de inteiros) para minutos (inteiro);
horasemminutos :: Hora -> Int 
horasemminutos (h,m) = h*60 + m 

--(d) converter um valor em minutos para horas;
minutosParaHora :: Int -> Hora
minutosParaHora m = divMod m 60

--(e) calcular a diferen¸ca entre duas horas (cujo resultado deve ser o n´umero de minutos);
diferencahoras :: Hora -> Hora -> Int
diferencahoras (h,m) (h1,m1) =horasemminutos (h,m) - horasemminutos (h1,m1) 

--(f) adicionar um determinado n´umero de minutos a uma dada hora.
adicionaminutos :: Hora -> Int -> Hora 
adicionaminutos (h,m) n = (h + hr,mr)
    where (hr,mr) = minutosParaHora (m + n) 

-- 4 
data Horas = H Int Int 
 
--(a) testar se um par de inteiros representa uma hora do dia válida;
horaValida :: Horas -> Bool
horaValida (H h m) = 0 <= h && h <= 23 &&
                      0 <= m && m <= 59

--(b) testar se uma hora é ou não depois de outra (comparação);
horaDepoisDe :: Horas -> Horas -> Bool
horaDepoisDe (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)


--(c) converter um valor em horas (par de inteiros) para minutos (inteiro);
horaParaMinutos :: Horas -> Int
horaParaMinutos (H h m) = 60 * h + m


--(d) converter um valor em minutos para horas;
minutosParaHora1 :: Int -> Horas
minutosParaHora1 m = H a b
    where (a,b) = divMod m 60


--(e) calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos);
diferencaHoras :: Horas -> Horas -> Int
diferencaHoras (H h1 m1) (H h2 m2) = (h1 - h2) * 60 + (m1 - m2)


--(f) adicionar um determinado número de minutos a uma dada hora;
adicionaMinutos :: Horas -> Int -> Horas 
adicionaMinutos (H h m) n = H (h + hr) mr
    where H hr mr = minutosParaHora1 (m + n)

--5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

--(a) Defina a fun¸c˜ao next :: Semaforo -> Semaforo que calcula o pr´oximo estado de um sem´aforo.
next :: Semaforo -> Semaforo 
next Vermelho = Verde
next Verde = Amarelo 
next Amarelo = Vermelho 

-- (b) Defina a fun¸c˜ao stop :: Semaforo -> Bool que determina se ´e obrigat´orio parar num sem´aforo.
stop :: Semaforo -> Bool 
stop Vermelho = True 
stop Verde = False 
stop Amarelo = False  

-- (c) Defina a fun¸c˜ao safe :: Semaforo -> Semaforo -> Bool que testa se o estado de dois sem´aforos num cruzamento ´e seguro.
safe :: Semaforo ->Semaforo -> Bool 
safe Vermelho _ = True 
safe _ Vermelho = True 
safe _ _ = False 

--6 
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

--(a) posx :: Ponto -> Double que calcula a distˆancia de um ponto ao eixo vertical. 
posx :: Ponto -> Double 
posx (Cartesiano n m) = n 
posx (Polar n m ) = n * cos m 

-- (b) posy :: Ponto -> Double que calcula a distˆancia de um ponto ao eixo horizontal. 

posy :: Ponto -> Double 
posy (Cartesiano x y) = y 
posy (Polar r a) = r * sin a

-- (c) raio :: Ponto -> Double que calcula a distˆancia de um ponto `a origem
raio :: Ponto -> Double 
raio (Cartesiano x y ) = sqrt (x^2 + y^2)
raio (Polar r a) = r 

--(d) angulo :: Ponto -> Double que calcula o ˆangulo entre o vector que liga a origem ao ponto e o eixo horizontal.
angulo :: Ponto -> Double 
angulo (Cartesiano x y )= atan(x/y)
angulo (Polar r a) = a 

-- (e) dist :: Ponto -> Ponto -> Double que calcula a distˆancia entre dois pontos.
dist' :: Ponto -> Ponto -> Double 
dist' p1 p2 = sqrt ((x2-x1)^2 + (y2-y1)^2)
             where x1 = posx p1 
                   x2 = posx p2 
                   y1 = posy p1 
                   y2 = posy p2 

-- 7 
data Figura = Circulo Ponto Double
             | Retangulo Ponto Ponto
             | Triangulo Ponto Ponto Ponto
 deriving (Show,Eq)

-- (a) Defina a fun¸c˜ao poligono :: Figura -> Bool que testa se uma figura ´e um pol´ıgono
poligono :: Figura -> Bool 
poligono (Circulo _ _  ) = False 
poligono (Retangulo p1 p2) =if  posx p1 /= posx p2 && posy p1 /= posy p2 
                             then True 
                             else False
poligono ( Triangulo p1 p2 p3) = if  posx p1 /= posx p2 || 
                                     posx p2 /= posx p3 ||
                                     posx p3 /= posx p1 
                                     &&
                                     posy p1 /= posy p2 ||
                                     posy p2 /= posy p3 || 
                                     posy p3 /= posy p1 
                                 then True 
                                 else False 

--(b) Defina a fun¸c˜ao vertices :: Figura -> [Ponto] que calcula a lista dos v´ertices de uma figura.
vertices :: Figura -> [Ponto] 
vertices (Circulo _ _) = [] 
vertices (Retangulo p1 p2) = [p1 , Cartesiano (posx p1)(posy p2), p2 ,Cartesiano (posx p2)(posy p1)]
vertices (Triangulo p1 p2 p3) =[p1,p2,p3] 

-- (c) Complete a seguinte defini¸c˜ao cujo objectivo ´e calcular a ´area de uma figura: 
area :: Figura -> Double
{-area (Triangulo p1 p2 p3) = -- feito já na folha e está a dar erro
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron -}
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)
area (Circulo _ r) = pi * (r ^ 2)

--(d) Defina a fun¸c˜ao perimetro :: Figura -> Double que calcula o per´ımetro de uma figura.
perimetro' :: Figura -> Double 
perimetro' (Circulo _ r ) = 2 * pi * r 
perimetro' (Retangulo p1 p2) = abs (posx p1 -posx p2)*2 + abs ( posy p2 -posy p1)*2
perimetro' (Triangulo p1 p2 p3) = dist' p1 p2 + dist' p2 p3 + dist' p3 p1 

--8  
-- (a) isLower :: Char -> Bool, que testa se um Char ´e uma min´uscula. 
isLower' :: Char -> Bool 
isLower' c = ord c >= ord 'a' && ord c <= ord 'z' 

-- (b) isDigit :: Char -> Bool, que testa se um Char ´e um d´ıgito.
isDigit :: Char -> Bool
isDigit ch = ord ch >= ord '0' && ord ch <= ord '9'

-- (c) isAlpha :: Char -> Bool, que testa se um Char é uma letra. 
isAlpha :: Char -> Bool
isAlpha ch = isLower' ch || isUpper ch
    where isUpper ch = ord ch >= ord 'A' && ord ch <= ord 'Z'

-- (d) toUpper :: Char -> Char, que converte uma letra para a respectiva mai´uscula
toUpper :: Char -> Char 
toUpper ch = if isLower ch 
             then chr (ord ch - 32)
             else ch
-- (e) intToDigit :: Int -> Char, que converte um n´umero entre 0 e 9 para o respectivo d´ıgito.
intToDigit' :: Int -> Char 
intToDigit'  n = chr (n + 48) 

--(f) digitToInt :: Char -> Int, que converte um d´ıgito para o respectivo inteiro.
digitToInt :: Char -> Int 
digitToInt ch = ord ch - 48