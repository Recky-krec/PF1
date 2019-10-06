--funC :: [Int] -> [Int]
funC (x:y:t) = funC t
funC [x] = []
funC [] = []

--funD :: [a] -> [a]
funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h : dobros t)

numOcorre :: Char -> String -> Int
numOcorre letra [] = 0
numOcorre letra (head:tail) = if (letra == head) then 1 + numOcorre letra tail
    else numOcorre letra tail

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if (h>0) then positivos t else False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h>0 then (h: soPos t)
    else soPos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h<0) then h + somaNeg t
                else somaNeg t

tresUlt :: [a] -> [a]
tresUlt l = if length l <= 3 then l else tresUlt (tail l)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (h:t) = snd h : segundos t

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros el [] = False
nosPrimeiros el (h:t) = if (fst h) == el then True else nosPrimeiros el t

-- funçoes para mexer com triplos
fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, y, _) = y

thd3 :: (a,b,c) -> c
thd3 (_, _, z) = z

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos (h:t) = ( fst3 h + fst3 (sumTriplos t), snd3 h + snd3 (sumTriplos t), thd3 h + thd3 (sumTriplos t))
