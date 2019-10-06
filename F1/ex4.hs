module Main where

data Hora = H Int Int deriving (Show,Eq)

main = do
    putStrLn(show(H 0 15))