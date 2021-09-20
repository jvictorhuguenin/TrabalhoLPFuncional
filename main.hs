
data Data = MinhaData Int Int Int | DataComemorativa String Int Int Bool Bool
    deriving(Show)
    
dia :: Data -> Int
dia (MinhaData d _ _) = d 
dia (DataComemorativa _ dc _ _ _ ) = dc

mes :: Data -> Int
mes (MinhaData _ m _ ) = m
mes (DataComemorativa _ _ mc _ _ ) = mc

ano :: Data -> Int
ano (MinhaData _ _ a) = a

nome :: Data -> String 
nome (DataComemorativa nc _ _ _ _) = nc

feriado :: Data -> Bool
feriado (DataComemorativa _ _ _ f _) = f

feriadoGlobal :: Data -> Bool
feriadoGlobal (DataComemorativa _ _ _ _ fg) = fg

compara :: Data -> Data -> Int 
compara (MinhaData a b _ )(DataComemorativa _ d e _ _ )
    |(a == d) && (b == e) = 0
    |(a > d) && (b == e) = 1
    |(a < d) && (b == e) = -1
    |(b > e) = 1
    |(b < e) = -1
    |otherwise = -1
    
datasComemorativas :: Data -> [Data] -> [Data]
datasComemorativas (DataComemorativa a b c d e) y = y++[(DataComemorativa a b c d e)]

horasNaoTrabalhadas :: [Data] -> Int 
horasNaoTrabalhadas [] = 0
horasNaoTrabalhadas (x:xs)
    |feriado x = 8 + horasNaoTrabalhadas xs
    |otherwise = horasNaoTrabalhadas xs
    
toString :: Data -> [Char]
toString (MinhaData a b c) = "Data Atual: " ++ show a ++ "/" ++ show b ++ "/" ++ show c
toString (DataComemorativa a b c d e) = "Data Comemorativa: "  ++ a ++ "  Dia:" ++ show b ++ "/" ++ show c  ++ "   eh feriado? " ++ show d ++ "  " ++ " Eh global? " ++ show e  ++ "."    


main :: IO()
main = do
    let dataAtual = MinhaData 16 9 2021
    let natal = DataComemorativa "Natal" 25 12 True True
    let independenciaBrasil = DataComemorativa "Independencia Brasil" 7 9 True False
    
    putStrLn(show(toString dataAtual))
    putStrLn(show(toString natal))
    putStrLn(show(toString independenciaBrasil))
    
    let comp = compara dataAtual natal
    putStrLn (show comp)
    
    let listaDatasComemorativas = []
    let listaDatasComemorativas2 = datasComemorativas natal listaDatasComemorativas
    let listaDatasComemorativas3 = datasComemorativas independenciaBrasil listaDatasComemorativas2
    
    let naoTrabalha = horasNaoTrabalhadas listaDatasComemorativas3
    putStrLn(show naoTrabalha)