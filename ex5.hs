module Main where

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

next :: Semaforo -> Semaforo
next s
    | s == Verde = Amarelo
    | s == Amarelo = Vermelho
    | s == Vermelho = Verde

stop :: Semaforo -> Bool
stop s = if (s == Vermelho || s == Amarelo) then True else False

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = if(s1 == Verde && s2 == Verde) then False else True
main = do
    putStrLn(show(1))