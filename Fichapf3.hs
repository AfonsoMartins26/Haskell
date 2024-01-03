module TestePf.Fichapf3 where
import TestePf.Fichapf1 (dist')



--1 
data Hora = H Int Int deriving Show 
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

horaDepoisDe :: Hora -> Hora -> Bool
horaDepoisDe (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)

-- (a) Testar se uma etapa est´a bem constru´ıda (i.e., o tempo de chegada ´e superior ao de partida e as horas s˜ao v´alidas).

validahora :: Hora -> Bool
validahora (H h m) = h>=0 && h< 24 && m>=0 && m<60

etapavalida :: Etapa -> Bool
etapavalida (hi,hf) =  if validahora hi && validahora hf && horaDepoisDe hi hf 
                       then True 
                       else False  

{-(b) Testa se uma viagem est´a bem constru´ıda (i.e., se para cada etapa, o tempo de
chegada ´e superior ao de partida, e se a etapa seguinte come¸ca depois da etapa
anterior ter terminado).-}

viagemvalida :: Viagem -> Bool 
viagemvalida [] = True 
viagemvalida (h:t) =viagemvalidaaux (h:t) 


viagemvalidaaux :: Viagem -> Bool 
viagemvalidaaux [] = True 
viagemvalidaaux [h] = etapavalida h 
viagemvalidaaux ((hi,hf):(hp,_):t) =  etapavalida(hi,hf) && viagemvalidaaux t && horaDepoisDe hp hf 

--(c) Calcular a hora de partida e de chegada de uma dada viagem.
partidaEChegada :: Viagem -> (Hora,Hora)
partidaEChegada v = (hi,hf)
    where (hi,_) = head v
          (_,hf) = last v

--(d) Dada uma viagem v´alida, calcular o tempo total de viagem efectiva
tempoViagemEfetiva :: Viagem -> Hora
tempoViagemEfetiva [] = H 0 0
tempoViagemEfetiva ((h1,h2):t) = tempoviagem' (minutosParaHora1(diferencaHoras h2 h1)) (tempoViagemEfetiva t)

tempoViagem :: Viagem -> Hora
tempoViagem (h:t) = tempoviagem (horaviagem (h:t))

tempoviagem :: (Hora,Hora) -> Hora
tempoviagem (H h m,H h1 m1) = H (h + h1 + h2) (m+m1-(h2*60))
                   where h2 = div (m + m1) 60  

--(e) Calcular o tempo total de espera.
tempoviagem' :: Hora -> Hora -> Hora
tempoviagem' (H h m) (H h1 m1) = H (h + h1 + h2) (m+m1-(h2*60))
                   where h2 = div (m + m1) 60   
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (H h1 m1) (H h2 m2) = (h1 - h2) * 60 + (m1 - m2)

minutosParaHora1 :: Int -> Hora
minutosParaHora1 m = H a b
    where (a,b) = divMod m 60

tempoEspera' :: Viagem -> Hora
tempoEspera' ((h1,h2):(h3,h4):t) = tempoviagem' (minutosParaHora1(diferencaHoras h3 h2)) (tempoEspera' ((h3,h4):t))
tempoEspera' _ = H 0 0

--(f) Calcular o tempo total da viagem (a soma dos tempos de espera e de viagemefectiva).
horaviagem :: Viagem -> (Hora,Hora) 
horaviagem (h:t)= (fst h , snd (last t))


--2
type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq) 

--(a) Defina a fun¸c˜ao para calcular o comprimento de uma linha poligonal.

posy :: Ponto -> Double 
posy (Cartesiano x y) = y 
posy (Polar r a) = r * sin a

posx :: Ponto -> Double 
posx (Cartesiano n m) = n 
posx (Polar n m ) = n * cos m 

dist :: Ponto -> Ponto -> Double 
dist p1 p2 = sqrt ((x2-x1)^2 + (y2-y1)^2)
             where x1 = posx p1 
                   x2 = posx p2 
                   y1 = posy p1 
                   y2 = posy p2 

comprimento :: Poligonal -> Double
comprimento (p1:p2:t) = dist p1 p2 + comprimento (p2:t)
comprimento _ = 0
                   
--(b) Defina uma fun¸c˜ao para testar se uma dada linha poligonal ´e ou n˜ao fechada.
linhafechada :: Poligonal -> Bool 
linhafechada p = length p >= 3 && head  p == last p 

{-(c) Defina a fun¸c˜ao triangula :: Poligonal -> [Figura] que, dada uma linha
poligonal fechada e convexa, calcule uma lista de triˆangulos cuja soma das ´areas
seja igual `a ´area delimitada pela linha poligonal. O tipo Figura ´e idˆentico ao
definido na Ficha 1 -}
data Figura = Circulo Ponto Double
             | Retangulo Ponto Ponto
             | Triangulo Ponto Ponto Ponto
 deriving (Show,Eq)

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:ps)
    | p1 == p3 = []
    | otherwise = Triangulo p1 p2 p3 : triangula (p1:p3:ps)
triangula _ = []

--(d) Defina uma fun¸c˜ao para calcular a ´area delimitada por uma linha poligonal fechada e convexa.
area :: Figura -> Double
area (Triangulo p1 p2 p3) = -- feito já na folha e está a dar erro
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron 
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)
area (Circulo _ r) = pi * (r ^ 2)


areaPol :: Poligonal -> Double
areaPol p = areaTris (triangula p)

areaTris :: [Figura] -> Double
areaTris [] = 0
areaTris (h:t) = area h + areaTris t

{-(e) Defina a fun¸c˜ao mover :: Poligonal -> Ponto -> Poligonal que, dada uma
linha poligonal e um ponto, d´a como resultado uma linha poligonal idˆentica `a
primeira mas tendo como ponto inicial o ponto dado.-}

mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p:pol 

{-(f) Defina a fun¸c˜ao zoom :: Double -> Poligonal -> Poligonal que, dada um
factor de escala e uma linha poligonal, dˆe como resultado uma linha poligonal
semelhante e com o mesmo ponto inicial mas em que o comprimento de cada
segmento de recta ´e multiplicado pelo factor dado.-}

zoom :: Double -> Poligonal -> Poligonal
zoom z (h:t) = mover (doZoom z h t) h 

doZoom :: Double -> Ponto -> Poligonal -> Poligonal 
doZoom z p [] = [] 
doZoom z p (h:t) = Cartesiano ((x - xp) * z + xp) ((y - yp) * z + yp) : doZoom z p t
    where x = posx h
          y = posy h
          xp = posx p
          yp = posy p


--3 
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

{-(a) Defina a fun¸c˜ao acrescEmail :: Nome -> String -> Agenda -> Agenda que,
dado um nome, um email e uma agenda, acrescenta essa informa¸c˜ao `a agenda.


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [email])]
acrescEmail nome email ((n,cs):t)
    | nome == n = (n, email:cs) : t
    | otherwise = (n,cs) : acrescEmail nome email t  
-}
{-(b) Defina a fun¸c˜ao verEmails :: Nome -> Agenda -> Maybe [String] que, dado
um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se esse
nome n˜ao existir na agenda a fun¸c˜ao deve retornar Nothing.-}

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing 
verEmails nome ((n,cs):t)
   | nome == n = Just (soEmails cs ) 
   | otherwise = verEmails nome t 


soEmails :: [Contacto] -> [String]
soEmails [] = []
soEmails (Email e : t) = e : soEmails t
soEmails (_:t) = soEmails t

{-(c) Defina a fun¸c˜ao consTelefs :: [Contacto] -> [Integer] que, dada uma lista
de contactos, retorna a lista de todos os n´umeros de telefone dessa lista (tanto
telefones fixos como telem´oveis).-}

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = [] 
consTelefs (Email _ :cs ) = consTelefs cs 
consTelefs ( Tlm c:cs) = c : consTelefs cs 


{-(d) Defina a fun¸c˜ao casa :: Nome -> Agenda -> Maybe Integer que, dado um nome
e uma agenda, retorna o n´umero de telefone de casa (caso exista).-}

casa :: Nome -> Agenda -> Maybe Integer
casa _ []  = Nothing 
casa nome ((n,cs):t) 
    | nome == n = numCasa cs 
    | otherwise = casa nome t 


numCasa :: [Contacto] -> Maybe Integer 
numCasa [] = Nothing 
numCasa (Casa n : t) = Just n 
numCasa (_ : t) = numCasa t


--4 
type Dia = Int
type Mes = Int
type Ano = Int
type Nomes = String
data Data = D Dia Mes Ano
      deriving Show
type TabDN = [(Nome,Data)]

--(a) Defina a fun¸c˜ao procura :: Nome -> TabDN -> Maybe Data, que indica a data de nascimento de uma dada pessoa, caso o seu nome exista na tabela.
procura :: Nome -> TabDN -> Maybe Data
procura name [] = Nothing 
procura name ((n,nasc):t) 
    | name == n = Just nasc 
    |otherwise = procura name t 

{-(b) Defina a fun¸c˜ao idade :: Data -> Nome -> TabDN -> Maybe Int, que calcula
a idade de uma pessoa numa dada data-}

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing 
idade (D dx mx ax) name ((n,D d m a):t)
   | name == n = Just (calculaidade' (D dx mx ax) (D d m a) )
   | otherwise = idade (D dx mx ax) name t 

calculaidade' :: Data -> Data -> Int 
calculaidade' (D dn mn an) (D d m a) = if m > mn || m == mn && d > dn 
                                      then abs (a - an) 
                                      else abs (a - an - 1)

--(c) Defina a fun¸c˜ao anterior :: Data -> Data -> Bool, que testa se uma data ´e anterior a outra data.
anterior :: Data -> Data -> Bool
anterior (D dn mn an) (D d m a) = if an < a || an == a && mn < m || an == a && mn == m && an < a 
                                  then True 
                                  else False 

--(d) Defina a fun¸c˜ao ordena :: TabDN -> TabDN, que ordena uma tabela de datas de nascimento, por ordem crescente das datas de nascimento.
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = inseredata  (n,d) (ordena t)   

inseredata :: (Nome,Data) -> TabDN -> TabDN 
inseredata (n,d) [] = [(n,d)]
inseredata (n,d) ((n1,d1):t) 
        | anterior d d1 == True = (n,d):((n1,d1):t) 
        |otherwise = (n1,d1) : inseredata (n,d) t 

{-(e) Defina a fun¸c˜ao porIdade:: Data -> TabDN -> [(Nome,Int)], que apresenta o
nome e a idade das pessoas, numa dada data, por ordem crescente da idade das
pessoas.-}
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade (D d m a) tabela = porIdadeAux (D d m a) (ordena tabela)

porIdadeAux :: Data -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux d ((nh,dh):t) = porIdadeAux d t ++ [(nh, calculaidade' dh d)] 

--5
data Movimento = Credito Float | Debito Float
                  deriving Show
data Datas = Data Int Int Int
                    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

{-(a) Construa a fun¸c˜ao extValor :: Extracto -> Float -> [Movimento] que produz 
uma lista de todos os movimentos (cr´editos ou d´ebitos) superiores a um determinado valor.-}

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext si ((_,_,mov):t)) valor = if getValor mov > valor   --si saldo inicial 
                                        then mov : extValor (Ext si t) valor 
                                        else extValor (Ext si t) valor

getValor :: Movimento -> Float
getValor (Credito x) = x
getValor (Debito x) = x

{-(b) Defina a fun¸c˜ao filtro :: Extracto -> [String] -> [(Data,Movimento)] que
retorna informa¸c˜ao relativa apenas aos movimentos cuja descri¸c˜ao esteja inclu´ıda
na lista fornecida no segundo parˆametro.-}

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext si ((date,descricao,mov):t)) listastr = if descricao `elem` listastr 
                                                    then (date,mov): filtro (Ext si t) listastr 
                                                    else  filtro (Ext si t) listastr

{-(c) Defina a fun¸c˜ao creDeb :: Extracto -> (Float,Float), que retorna o total de
cr´editos e de d´ebitos de um extracto no primeiro e segundo elementos de um par,
respectivamente.-}
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext si ((_,_,Credito x):t)) = (x + cr, dr)
    where (cr,dr) = creDeb (Ext si t)
creDeb (Ext si ((_,_,Debito x):t)) = (cr, x + dr)
    where (cr,dr) = creDeb (Ext si t)

creDeb' :: Extracto -> (Float,Float)
creDeb' (Ext _ []) = (0,0)
creDeb' (Ext si ((_,_,mov):t)) = (c + cr, d + dr)
    where (cr,dr) = creDeb (Ext si t)
          (c,d) = case mov of Credito x -> (x,0)
                              Debito x -> (0,x)

{-(d) Defina a fun¸c˜ao saldo :: Extracto -> Float que devolve o 
saldo final que resulta da execu¸c˜ao de todos os movimentos no extracto sobre o saldo inicial.-}

saldo :: Extracto -> Float
saldo (Ext si []) = si
saldo (Ext si ((date,descricao,Credito x):t))= saldo (Ext (si + x) t )
saldo (Ext si ((date,descricao,Debito x):t))= saldo (Ext (si-x) t )
