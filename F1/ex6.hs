module Main where

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x1 _) = x1
posx (Polar d teta) = cos(teta) * abs(d)

posy :: Ponto -> Double
posy (Cartesiano _ y1) = y1
posy (Polar d teta) = sin(teta) * abs(d)

raio :: Ponto -> Double
raio (Cartesiano x1 y1) = sqrt(x1^2 + y1^2)
raio (Polar d _) = d

angulo :: Ponto -> Double
angulo (Cartesiano x1 y1) = atan(y1/x1)
angulo (Polar _ teta) = teta

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1)(Cartesiano x2 y2) = sqrt((y2-y1)^2 + (x2-x1)^2)
dist (Polar d1 teta1)(Polar d2 teta2) = sqrt( (posy(Polar d1 teta1) - posy(Polar d2 teta2)) + (posx(Polar d1 teta1) - posx(Polar d2 teta2)) )

main = do
    putStrLn(show( posx(Polar (1) (2*pi)  )))
    putStrLn(show( posy(Polar (1) (pi/2)  )))
    putStrLn(show( posx(Cartesiano (5) (20)  )))
    putStrLn(show( posy(Cartesiano (5) (20)  )))
    putStrLn(show(raio(Cartesiano 7 7)))
    putStrLn(show(angulo(Polar 1 pi)))
    putStrLn(show(dist(Polar 1 pi)(Polar 2 pi)))