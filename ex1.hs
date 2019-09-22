module Main where

perimetroCirculo :: Double -> Double
perimetroCirculo r = 2*pi*r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt( (x2+x1)^2 + (y2-y1)^2 )

primUlt :: [Int] -> [Int]
primUlt l
    | length(l) == 0 = []
    | length(l) == 1 = l
    | length(l) >  1 = [head(l), last(l)]

multiplo :: Int -> Int -> Bool
multiplo m n = if mod n m == 0 then False else True

truncaImpar :: [Int] -> [Int]
truncaImpar l
    | length(l) == 0 = []
    | mod (length l) 2 == 0 = l
    | mod (length l) 2 /= 0 = tail(l)

max2 :: Double -> Double -> Double
max2 a b = if a > b then a else b

max3 :: Double -> Double -> Double -> Double
max3 a b c = max2 c (max2 a b)

main = do
    putStrLn ("Perimetro: " ++ show(perimetroCirculo(5)))
    putStrLn ("Distancia A a B: " ++ show( dist(0,0)(1,1) ))
    putStrLn ("Primeiro e Ultimo: " ++ show( primUlt[1,2,3,4,5] ))
    putStrLn ("10 é multiplo de 5: " ++ show( multiplo 10 5 ))
    putStrLn ("truncaImpar" ++ show( truncaImpar [1,2,3] ))
    putStrLn ( show( max2 23 29 ))
    putStrLn ( show( max3 23 29 30))