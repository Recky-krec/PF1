module Main where
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo (Cartesiano x1 y1) r) = if r <= 0 then False else True
poligono (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2) ) = if (x1 == x2 && y1 == y2) then False else True
poligono (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = if ((y2-y1)/(x2-x1)) == ((y3-y2)/(x3-x2)) then False else True

main = do
    putStrLn(show(poligono (Triangulo (Cartesiano 1 1) (Cartesiano 2 2) (Cartesiano 3 3)) ))
