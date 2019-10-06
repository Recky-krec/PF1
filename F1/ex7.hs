module Main where
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x1 _) = x1
posx (Polar d teta) = cos(teta) * abs(d)

posy :: Ponto -> Double
posy (Cartesiano _ y1) = y1
posy (Polar d teta) = sin(teta) * abs(d)

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1)(Cartesiano x2 y2) = sqrt((y2-y1)^2 + (x2-x1)^2)
dist (Polar d1 teta1)(Polar d2 teta2) = sqrt( (posy(Polar d1 teta1) - posy(Polar d2 teta2)) + (posx(Polar d1 teta1) - posx(Polar d2 teta2)) )

poligono :: Figura -> Bool
poligono (Circulo (Cartesiano x1 y1) r) = if r <= 0 then False else True
poligono (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2) ) = if (x1 == x2 && y1 == y2) then False else True
poligono (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = if ((y2-y1)/(x2-x1)) == ((y3-y2)/(x3-x2)) then False else True

-- retangulo = [esq cima, esq baixo, direita cima, direita baixo]
vertices :: Figura -> [Ponto]
vertices (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2))
    | y1 > y2 = [Cartesiano x1 y1, Cartesiano x1 y2, Cartesiano x2 y1, Cartesiano x2 y2] -- p1 e o esq cima
    | y1 < y2 = [Cartesiano x2 y2, Cartesiano x2 y1, Cartesiano x1 y2, Cartesiano x1 y1] -- p1 e o direita baixo

area :: Figura -> Double
area (Triangulo p1 p2 p3) = 
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt(s*(s-a)*(s-b)*(s-c)) -- formula de heron
area (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = abs(y1-y2) * abs(x1-x2)
area (Circulo _ r) = pi * r^2

perimetro :: Figura -> Double
perimetro (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = 2*abs(y1-y2) + 2*abs(x1-x2)
perimetro (Circulo _ r) = 2*pi*r
perimetro (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
    in a+b+c


main = do
    putStrLn(show(poligono (Triangulo (Cartesiano 1 1) (Cartesiano 2 2) (Cartesiano 3 3)) ))
    putStrLn(show(area (Triangulo (Cartesiano 1 1) (Cartesiano 2 2) (Cartesiano 3 1)) ))
    putStrLn(show(perimetro (Triangulo (Cartesiano 1 1) (Cartesiano 2 2) (Cartesiano 3 1) ) ))