-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g164 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving (Show)

polarToCartesiano :: Vetor -> Vetor
polarToCartesiano (Polar r b) = Cartesiano x y
    where 
        x = r*cos((b*pi)/180.0)
        y = r*sin((b*pi)/180.0)
polarToCartesiano (Cartesiano x y) = Cartesiano x y 
        

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1+x2) (y1+y2))
somaVetores (Polar r a)        (Polar r1 a1)      = somaVetores (polarToCartesiano (Polar r a)) (polarToCartesiano (Polar r1 a1))
somaVetores (Cartesiano x y)   (Polar r a)        = somaVetores (polarToCartesiano (Polar r a)) (Cartesiano x y)
somaVetores (Polar r a)        (Cartesiano x y)   = somaVetores (polarToCartesiano (Polar r a)) (Cartesiano x y)


-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1-x2) (y1-y2))
subtraiVetores (Polar r a)        (Polar r1 a1)      = subtraiVetores (polarToCartesiano (Polar r a)) (polarToCartesiano (Polar r1 a1))
subtraiVetores (Cartesiano x y)   (Polar r a)        = subtraiVetores (Cartesiano x y) (polarToCartesiano (Polar r a))
subtraiVetores (Polar r a)        (Cartesiano x y)   = subtraiVetores (polarToCartesiano (Polar r a)) (Cartesiano x y)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor k (Cartesiano x1 y1) = (Cartesiano (k*x1) (k*y1))
multiplicaVetor k (Polar r a)        = multiplicaVetor k (polarToCartesiano (Polar r a))



-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
--intersetam :: Reta -> Reta -> (Double, Double)
--intersetam ((Cartesiano x1 y1), (Cartesiano x2 y2)) ((Cartesiano x3 y3), (Cartesiano x4 y4)) = if (t1 >= 0 && t1 <= 1) && (t2 >= 0 && t2 <= 1) then (1,1) else (t1,t2)
--   where
--        d  = ((x4-x3)*(y1-y2))-((x1-x2)*(y4-y3))
--        t1 = ((y3-y4)*(x1-x3))+((x4-x3)*(y1-y3))
--        t2 = ((y1-y2)*(x1-x3))+((x2-x1)*(y1-y3))
--intersetam (p1,p2) (p3,p4) = intersetam (polarToCartesiano p1, polarToCartesiano p2) (polarToCartesiano p3,polarToCartesiano p4)
intersetam :: Reta -> Reta -> Bool
intersetam x y = let (ta, tb) = aux x y in ta >= 0 && ta <= 1 && tb >= 0 && tb <= 1

aux :: Reta -> Reta -> (Double, Double)
aux (a, b) (c, d) = (ta, tb) 
    where
        (Cartesiano x1 y1) = polarToCartesiano a
        (Cartesiano x2 y2) = polarToCartesiano b
        (Cartesiano x3 y3) = polarToCartesiano c
        (Cartesiano x4 y4) = polarToCartesiano d
        ta = ((y3 - y4) * (x1 - x3) + (x4 - x3) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))
        tb = ((y1 - y2) * (x1 - x3) + (x2 - x1) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))

 

-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1@(Cartesiano x1 y1), p2@(Cartesiano x2 y2)) ((Cartesiano x3 y3), (Cartesiano x4 y4)) = somaVetores p1 (multiplicaVetor ta (subtraiVetores p2 p1))
   where 
      ta = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))

intersecao (p1,p2) (p3,p4) = intersecao (polarToCartesiano p1, polarToCartesiano p2) (polarToCartesiano p3, polarToCartesiano p4)      

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False
eIndiceListaValido n l  = (n>=0 && n< length l)

--eIndiceListaValido i l = if ((length(l) < i) || (i<0)) then False else True 


-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0) 
dimensaoMatriz m | c==0 = (0,0) 
                 |otherwise = (l,c)
                      where l = length m
                            c = length (head m)


-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (y,x) m = if (y>=y1 || y<0 || x>=x1 || x< 0) then False else True
    where
        (y1,x1) = dimensaoMatriz(m)

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
--normalizaAngulo a = if (a < 0 || a >= 360) then rem (abs(a)) 360 else a

normalizaAngulo a = if (a<360 && a>=0) then a 
                    else if (a>= 360) then normalizaAngulo (a-360)
                    else normalizaAngulo (a+360)

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista e (h:t) = encontraIndiceLista (e-1) t


--encontraIndiceLista n l = if n == 0 then head l else encontraIndiceLista (n-1) (tail l)

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista n el (h:t) = if n == 0 then (el:t) else h : (atualizaIndiceLista (n-1) el t)

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (m,n) matriz = if ePosicaoMatrizValida (m,n) matriz then encontraIndiceLista n (encontraIndiceLista m matriz)
    else error "Posicao Matriz invalida" 


-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) e m | ePosicaoMatrizValida (l,c) m = m'
                                | otherwise = m 
  where ls = encontraIndiceLista l m
        ls' = atualizaIndiceLista c e ls
        m' = atualizaIndiceLista l ls' m 



-- = if (ePosicaoMatrizValida (m,n) m == False) then m
--                                        else  atualizaIndiceLista m (atualizaIndiceLista n x (encontraPosicaoMatriz))


