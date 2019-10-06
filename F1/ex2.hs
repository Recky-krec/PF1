module Main where

nRaizes :: Double -> Double -> Double -> Double
nRaizes a b c
    | binomio > 0 = 2
    | binomio == 0 = 1
    | binomio < 0 = 0
    where
        binomio = b^2 - 4*a*c

raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    | (nRaizes a b c) == 2 = [pos, neg]
    | (nRaizes a b c) == 1 = [pos]
    | (nRaizes a b c) == 0 = []
    where
        binomio = b^2 - 4*a*c
        pos = (-b + sqrt(binomio)) / (2*a)
        neg = (-b - sqrt(binomio)) / (2*a)

main = do
    putStrLn( show(nRaizes 1 2 1) )
    putStrLn( show(raizes 1 2 1) )