module Main where

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo min max = if min > max then [] else min : myEnumFromTo(min+1) max

main = do
    putStrLn( show(myEnumFromTo 2 7) )

-- f min max = if min > max then [] else min : f (min+1) max

