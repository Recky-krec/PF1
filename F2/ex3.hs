import Data.Char

--eDigito :: Char -> Bool
--eDigito h = if (h == '0' || h == '1' || h == '2' || h == '3' || h == '4' || 
--				h == '5' || h == '6' || h == '7' || h == '8' || h == '9') then True
--			else False

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if isDigit h then h : soDigitos t else soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if isLower h then 1 + minusculas t else minusculas t

nums :: String -> [Int]
nums [] = []
nums (h:t) = if isDigit h then (digitToInt h) : nums t else nums t
