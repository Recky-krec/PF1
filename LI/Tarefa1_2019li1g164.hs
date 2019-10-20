-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g164 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = []

-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

{- 
Mapas válidos:
1. Todas as pistas começam com Recta Terra 0;
2. Todas as pistas têm o mesmo comprimento
3. Não há alturas negativas
4. As alturas inicial e final de peças adjacentes na mesma pista são iguais. 
   Por exemplo, a altura final de uma peça do tipo rampa 
   terá que ser igual à altura inicial da peça que lhe sucede
-}

gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento seed = geraAux npistas comprimento seed paresRand 0
        where
            paresRand = (pairElems (geraAleatorios (2*npistas*(comprimento-1)) seed) )


geraAux :: Int -> Int -> Int -> [(Int,Int)] -> Int -> Mapa
geraAux npistas comprimento seed paresRand n   | comprimento == 1 = (replicate npistas ([Recta Terra 0]))
                                               | paresRandPista == [] = []
                                               | otherwise = geraPista comprimento paresRandPista : geraAux (npistas-1) comprimento seed paresRand (n+1)
        where
            paresRandPista = (take (comprimento-1) (drop (n*(comprimento-1)) paresRand))

-- uma pista de comprimento 5 gera na verdade 4 peças porque a primeira é sempre a (Reta Terra 0)
geraPista :: Int -> [(Int, Int)] -> Pista
geraPista comprimento [] = []
geraPista comprimento l = Recta Terra 0 : geraPistaAux (comprimento-1) l (Recta Terra 0)

geraPistaAux :: Int -> [(Int, Int)] -> Peca -> Pista
geraPistaAux 0 l pecaAnt = []
geraPistaAux comprimento ((f,s):t) pecaAnt = let pecaCurrente = (geraPeca pecaAnt (f,s)) in pecaCurrente : (geraPistaAux (comprimento-1) t pecaCurrente)



-- hAnt = altura peça anterior
-- (gamaTipo + 1) = Se a gama for 0, a rampa sobre 1 nivel, se a game for 1, a rampa sobe 2 de altura
geraPeca :: Peca -> (Int, Int) -> Peca
geraPeca pecaAnt (gamaPiso,gamaTipo)  | (gamaTipo == 0 || gamaTipo == 1)              = (Rampa (piso) (hAnt) (hAnt + (gamaTipo+1)) )
                                      | (gamaTipo >= 2 && gamaTipo <= 5 && hAnt /= 0) = (Rampa (piso) (hAnt) (hAnt - 1)) -- descde sempre de 1 piso, mudar aqui
                                      | (gamaTipo >= 2 && gamaTipo <= 5 && hAnt == 0) = (Recta (piso) (hAnt))
                                      | (gamaTipo >= 6 && gamaTipo <= 9)              = (Recta (piso) hAnt)
                            where 
                                hAnt = getAlturaFinalPeca pecaAnt
                                piso = determinaPiso pecaAnt gamaPiso


-- Dado um valor randomico, devolve o piso correspondente
determinaPiso :: Peca -> Int -> Piso
determinaPiso pecaAnt gamaPiso      | (gamaPiso == 0 || gamaPiso == 1) = Terra
                                    | (gamaPiso == 2 || gamaPiso == 3) = Relva
                                    | (gamaPiso == 4)                  = Lama
                                    | (gamaPiso == 5)                  = Boost
                                    | otherwise                        = getPisoPeca(pecaAnt)

-- Devolve o piso correspondente a peca dada
getPisoPeca :: Peca -> Piso
getPisoPeca (Recta piso _) = piso
getPisoPeca (Rampa piso _ _) = piso

-- Define um novo piso para a peca dada
setPisoPeca :: Piso -> Peca -> Peca
setPisoPeca novoPiso (Recta p h) = (Recta novoPiso h)
setPisoPeca novoPiso (Rampa p hi hf) = (Rampa novoPiso hi hf)

-- Devolve a altura inicial correspondente a peca dada
getAlturaInicialPeca :: Peca -> Int
getAlturaInicialPeca (Recta _ h) = h
getAlturaInicialPeca (Rampa  _ hi _) = hi

-- Devolve a altura final correspondente a peca dada
getAlturaFinalPeca :: Peca -> Int
getAlturaFinalPeca (Recta _ h) = h
getAlturaFinalPeca (Rampa  _ _ hf) = hf

-- Agrupa 2 a 2 uma lista com o numero par de elementos
pairElems :: [Int] -> [(Int, Int)]
pairElems [] = []
pairElems (h:s:t) = if mod (length (h:s:t)) 2 == 0 then ((h,s):pairElems t)
                                      else error "o numero de elementos é impar"


{-type Mapa = [Pista]

type Pista = [Peca]

data Peca = Recta Piso Int | Rampa Piso Int Int deriving (Read,Show,Eq)

data Piso = Terra | Relva | Lama | Boost | Cola


m = [[Recta Terra 0,Recta Terra 0, Rampa Lama 0 3, Rampa Lama 3 1, Rampa Terra 1 0] 
    ,[ Recta Terra 0,Rampa Cola 0 2 , Rampa Cola 2 0, Recta Boost 0, Rampa Boost 0 3]
    ,[ Recta Tera 0,Rampa Relva 0 , Rampa Lama  , Rampa Lama , Recta Boost  ]
    ,[ Recta Terra 0,Recta Lama 0, Recta Relva  ,Rampa Boost  , Recta Cola ]]

Tarefa1> geraAleatorios 1
[2,4,7,9,5,1,0,3,8,6,3,2,0,4,0,1,6,0,4,1,7,3,5,8,0,2,5,3,7,4,1,0]-}

