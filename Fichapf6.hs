module TestePf.Fichapf6 where

--1
data BTree a = Empty
          | Node a (BTree a) (BTree a)
     deriving Show

arvore = (Node 5 (Node 2 (Node 1 Empty
                                 Empty) 
                         (Node 3 Empty 
                                 Empty)) 
                 (Node 9 (Node 7 (Node 6 Empty 
                                         Empty) 
                                 (Node 8 Empty 
                                         Empty)) 
                         Empty))

--(a) altura :: BTree a -> Int que calcula a altura da ´arvore.
altura :: BTree a -> Int
altura Empty = 0 
altura (Node _ l r ) = 1 + max (altura l) ( altura r )

--(b) contaNodos :: BTree a -> Int que calcula o n´umero de nodos da ´arvore

contaNodos :: BTree a -> Int
contaNodos Empty = 0 
contaNodos (Node x l r) = 1 + contaNodos l + contaNodos r 

{- (c) folhas :: BTree a -> Int, que calcula 
o n´umero de folhas (i.e., nodos sem descendentes) da ´arvore.-}

folhas :: BTree a -> Int 
folhas Empty = 0 
folhas (Node _ Empty Empty ) = 1
folhas (Node _ l r) = folhas l + folhas r

{-(d) prune :: Int -> BTree a -> BTree a, que remove de uma 
´arvore todos os elementos a partir de uma determinada profundidade.-}

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty 
prune _ Empty = Empty 
prune n (Node e l r ) = Node e (prune (n-1) l) (prune(n-1) r) 

{-(e) path :: [Bool] -> BTree a -> [a], que dado um caminho (False corresponde
a esquerda e True a direita) e uma ´arvore, d´a a lista com a informa¸c˜ao dos nodos
por onde esse caminho passa-}
path :: [Bool] -> BTree a -> [a]
path [] (Node e _ _) = [e] 
path _ Empty = []
path (h:t) (Node e l r ) = e : path t (if h then r else l)  


--(f) mirror :: BTree a -> BTree a, que d´a a ´arvore sim´etrica.
mirror :: BTree a -> BTree a
mirror Empty = Empty 
mirror (Node e l r) = (Node e (mirror r) (mirror l)) 

{-(g) zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
que generaliza a fun¸c˜ao zipWith para ´arvores bin´arias.-}
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node e l r)(Node e' l' r') = Node (f e e') (zipWithBT f l l')(zipWithBT f r r')
zipWithBT _ _ _ = Empty 

{-(h) unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c), 
que generaliza a
fun¸c˜ao unzip (neste caso de triplos) para ´arvores bin´arias.-}
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) l r) =(Node a unzipl1 unzipr1,Node b unzipl2 unzipr2,Node c unzipl3 unzipr3)
       where (unzipl1,unzipl2,unzipl3) = unzipBT l 
             (unzipr1,unzipr2,unzipr3) = unzipBT r

-- 2
{-(a) Defina uma fun¸c˜ao minimo :: Ord a => BTree a -> a que 
determina o menor elemento de uma ´arvore bin´aria de procura n˜ao vazia.
como numa arvore de procura • a raiz da árvore 
é maior do que todos os elementos que estão na sub-árvore esquerda e 
a raiz da árvore é menor do que todos os elementos que estão na sub-árvore direita
assim pra saber o menor elemento é ver o menor elemento da sub arvore esquerda -}

minimo :: Ord a => BTree a -> a
minimo (Node e Empty _ ) = e 
minimo (Node e l r) = minimo l 

{-(b) Defina uma fun¸c˜ao semMinimo :: Ord a => BTree a -> BTree a que remove o
menor elemento de uma ´arvore bin´aria de procura n˜ao vazia-}

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty r) = r -- neste cado a raiz vai ser o menor elemento 
semMinimo (Node e l r) = Node e (semMinimo l) r

{-(c) Defina uma fun¸c˜ao minSmin :: Ord a => BTree a -> (a,BTree a) que calcula,
com uma ´unica travessia da ´arvore o resultado das duas fun¸c˜oes anteriores.-}

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty r) = (e,r)
minSmin (Node e l r) = (a,Node e b r)
          where (a,b) = minSmin l 


{-(d) Defina uma fun¸c˜ao remove :: Ord a => a -> BTree a -> BTree a que remove
um elemento de uma ´arvore bin´aria de procura, usando a fun¸c˜ao anterior-}

remove :: Ord a => a -> BTree a -> BTree a-- nesta funcao tamos a meter a retirar o menor elemento e a 
remove _ Empty = Empty        -- colocar o menor elemento da direita na esquerda onde retiramos o menor elemento
remove x (Node e l r)           -- pois numa arvore de procura náo pode haver Nodes vazios 
    | x< e  = Node e (remove x l) r
    |x> e = Node e l (remove x r)
    |otherwise = case r of Empty -> l 
                           _ -> let (g,h) = minSmin r 
                              in Node g l h 

--3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
    deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

turma :: Turma
turma = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) (Node (20,"Pedro",TE,Aprov 10) Empty (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty) (Node (28,"Vasco",MEL,Rep) Empty Empty))))
--(a) inscNum :: Numero -> Turma -> Bool, que verifica se um aluno, com um dado n´umero, est´a inscrito.
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False 
inscNum n (Node (num,_,_,_) l r) 
    | n == num = True 
    |otherwise =  inscNum n (if n < num then l else r)

{-(b) inscNome :: Nome -> Turma -> Bool, que verifica se um aluno, com um dado
nome, est´a inscrito-}
inscNome :: Nome -> Turma -> Bool
inscNome name Empty = False 
inscNome name (Node (_,n,_,_) l r)
    | name == n = True 
    | otherwise = inscNome name l || inscNome name r 

{-(c) trabEst :: Turma -> [(Numero,Nome)], que lista o n´umero e nome dos alunos
trabalhadores-estudantes (ordenados por n´umero).-}
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,name,TE,_) l r) =trabEst l ++ [(num,name)] ++trabEst r 
trabEst (Node _ l r) = trabEst l ++ trabEst r 

{-(d) nota :: Numero -> Turma -> Maybe Classificacao, que calcula a classifica¸c˜ao
de um aluno (se o aluno n˜ao estiver inscrito a fun¸c˜ao deve retornar Nothing).-}

nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (num,_,_,clas) l r)
  | n == num = Just clas
  | n < num = nota n l 
  |otherwise = nota n r 
nota _ _  = Nothing 

{-(e) percFaltas :: Turma -> Float, que calcula a percentagem de 
alunos que faltaram `a avalia¸c˜ao.-}

percFaltas :: Turma -> Float 
percFaltas Empty = 0 
percFaltas turma = (sumFaltas turma / numAlunos turma) * 100
    where sumFaltas :: Turma -> Float
          sumFaltas Empty = 0
          sumFaltas (Node (_,_,_,Faltou) l r) = 1 + sumFaltas l + sumFaltas r
          sumFaltas (Node _ l r) = sumFaltas l + sumFaltas r
          numAlunos = fromIntegral . contaNodos -- cada Nodo representa um aluno 


{-(f) mediaAprov :: Turma -> Float, que calcula a m´edia das notas dos alunos que
passaram.-}

mediaAprov :: Turma -> Float 
mediaAprov Empty = 0 
mediaAprov turma = uncurry (/) (sumNumNotas turma)
    where sumNumNotas :: Turma -> (Float, Float)
          sumNumNotas Empty = (0,0)
          sumNumNotas (Node (_,_,_,Aprov nota) l r) = addPairs (fromIntegral nota, 1) (addPairs (sumNumNotas l) (sumNumNotas r))
          sumNumNotas (Node _ l r) = addPairs (sumNumNotas l) (sumNumNotas r)
          addPairs (a,b) (c,d) = (a+c,b+d)

{-(g) aprovAv :: Turma -> Float, que calcula o r´acio de alunos aprovados por avaliados.
 Implemente esta fun¸c˜ao fazendo apenas uma travessia da ´arvore.-}

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = uncurry (/) (sumAprovAv turma)
          
sumAprovAv :: Turma -> (Float, Float)
sumAprovAv Empty = (0,0)
sumAprovAv (Node (_,_,_,clas) l r) = case clas of Aprov nota -> (ap+1,av+1) 
                                                  Rep -> (ap,av+1)
                                                  _ -> (ap,av)
    where (ap,av) = addPairs (sumAprovAv l) (sumAprovAv r)
          addPairs (a,b) (c,d) = (a+c,b+d)

