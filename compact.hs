import Data.List

a `extrayendo` b = (drop (length b) a)

a `seRepiteEn` b = a `isPrefixOf` (b `extrayendo` a)

cuantasVecesSeRepite a b | a `isPrefixOf` b = 1 + cuantasVecesSeRepite a (b `extrayendo` a)
                         | otherwise = 0

n `tirasInicialesPosibles` lista = drop n (reverse (inits lista))

mayorTiraInicialRepetida lista = head (filter (`seRepiteEn` lista) tirasInicialesRepetiblesPosibles)
                                 where tirasInicialesRepetiblesPosibles =  mitadTamañoLista `tirasInicialesPosibles` lista
                                       mitadTamañoLista = (length lista `quot` 2)

tirasRepetidas :: Eq a => [a] -> [[a]]
tirasRepetidas [] = []
tirasRepetidas lista | null mayorTiraInicial = (take 1 lista) : (tirasRepetidas (drop 1 lista))
                     | otherwise = mayorTiraInicial : tirasRepetidas (iterate (lista `extrayendo`) mayorTiraInicial !! (cuantasVeces))
                       where mayorTiraInicial = mayorTiraInicialRepetida lista
                             cuantasVeces = cuantasVecesSeRepite mayorTiraInicial lista

tirasRepetidasSegmentadas lista = map concat (tirasRepetidas (group lista))
