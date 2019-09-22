module Main where
    
type Hora = (Int, Int)

eValida :: Hora -> Bool
eValida (h,m) = if (h > 23 || h < 0 || m > 59 || m < 0) then False
    else True

horasParaMinutos :: Hora -> Int
horasParaMinutos (h,m) = 60*h + m

minutosParaHoras :: Int -> Hora
minutosParaHoras m = (horas, minutosRestantes)
    where
        horas = div m 60
        minutosRestantes = m - 60*horas

diferencaDeHoras :: Hora -> Hora -> Int
diferencaDeHoras (h1,m1)(h2,m2) = abs(a-b)
    where
        a = horasParaMinutos(h1,m1)
        b = horasParaMinutos(h2,m2)
adicionarMinutos :: Hora -> Int -> Hora
adicionarMinutos (h1,m1) m = if (h1 + horasParaSomar) >= 24 then ((h1 + horasParaSomar) - 24, minutosDeDiferenca)
    else (h1 + horasParaSomar, minutosDeDiferenca)
    where
        minutosParaSomar = m1 + m
        horasParaSomar = div minutosParaSomar 60
        minutosDeDiferenca = minutosParaSomar - (horasParaSomar * 60)

main = do
    putStrLn( show( eValida(00,00) ))
    putStrLn( show( minutosParaHoras( horasParaMinutos(4,15) )))
    putStrLn( show( diferencaDeHoras(12,40)(15,45) ))
    putStrLn( show( adicionarMinutos(12,45) 720))