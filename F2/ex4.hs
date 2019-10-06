type Polinomio = [Monomio]
type Monomio = (Float, Int)

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n (h:t) = if snd h == n then 1 + conta n t else conta n t

grau :: Polinomio -> Int
grau [] = 0
grau (h:t) = if snd h > grau t then snd h else grau t

selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n (h:t) = if (snd h == n) then (h : selgrau n t) else selgrau n t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (h:t) = ( fst h * fromIntegral(snd h - 1), (snd h - 1) ) : deriv t  

-- exceçao se o expoente for negativo
calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x (h:t) = ((fst h) * (x ^ (snd h))) + (calcula x t)

-- retira de um polinomio os monomios de coeficiente zero
simp :: Polinomio -> Polinomio
simp [] = []
simp (h:t) = if ( fst h == 0 ) then simp t else h : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult m [] = []
mult (f,s) (h:t) = ( (f * fst h), ( snd h + s) ) : mult (f,s) t 

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza(h:s:t) = undefined