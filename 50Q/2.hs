module Main where

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo fst snd last = if fst > last then [] else fst : myEnumFromThenTo snd (snd + (snd - fst)) last

main = do
    putStrLn( show(myEnumFromThenTo 1 3 10) )