module Pfaula3 where
import Data.Char
import Pftpc1 (horasemminutos)

data Hora = H Int Int deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

v :: Viagem 
v = [(H 9 30, H 10 25),
     (H 11 20, H 12 45),
     (H 13 30, H 14 45)]



validahora :: Hora -> Bool
validahora (H h m) = h>=0 && h< 24 && m>=0 && m<60

comparacaohora :: Hora ->  Hora -> Bool
comparacaohora (H h1 m1) (H h2 m2) = if h1  > h2 then True
                                     else if h1 >= h2 && m1 > m2 then True
                                    else False

etapavalida :: Etapa -> Bool
etapavalida (hi,hf) =  if validahora hi && validahora hf then True 
                       else False 


viagemValida :: Viagem -> Bool
viagemValida [] = False 
viagemValida [e] = etapavalida e  
viagemValida (e1:e2:es) = etapavalida e1 
                         && comparaEtapa e1 e2 
                         && viagemValida (e2:es)


comparaEtapa :: Etapa -> Etapa -> Bool
comparaEtapa e1 e2 = comparacaohora (fst e2)(snd e1)


horaPartidaChegada :: Viagem -> (Hora,Hora) 
horaPartidaChegada v = (fst (head v),snd (last v ))

tempoTotalviagem :: Viagem -> Int
tempoTotalviagem [] = 0 
tempoTotalviagem (x:xs) = etapaEmMinutos x + tempoTotalviagem xs  


diferencahoras :: Hora -> Hora -> Int
diferencahoras (H h1 m1)(H h2 m2) =  abs (h1-h2)*60 + (m1-m2)

etapaEmMinutos :: Etapa -> Int 
etapaEmMinutos h1 h2  = diferencahoras h1 h2 

tempoEmEspera :: Viagem -> Int 
tempoEmEspera [] = 0 
tempoEmEspera v = etapaEmMinutos ( horaPartidaChegada v)
                       tempoTotalviagem v 

tempoTotaldeViagem :: Viagem -> Int  
tempoTotaldeViagem v = 
        let (hi,hf) = horaPartidaChegada v 
        in etapaEmMinutos (hi,hf)

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String
data Data = D Dia Mes Ano deriving Show
type TabDN = [(Nome,Data)]


amigos :: TabDN
amigos = [("Zé" , D 10 10 2010)
         , ("Ana", D 5 5 2005)
         , ("Rui", D 1 1 2001)]

procura :: Nome -> TabDN -> Maybe Data 
procura n [] = Nothing 
procura n ((no,dn):as) 
            | n == no = Just dn 
            |otherwise = procura n as 


idade :: Data -> Nome -> TabDN -> Maybe Int 
idade _ _ [] = Nothing 
idade d n (...) 
