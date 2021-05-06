paridade:: [Int] -> [Bool]
paridade [] = []
paridade l = map even l

pega3 :: String -> String
pega3 x = take 3 x

prefixos :: [String] -> [String]
prefixos[] = []
prefixos l = map pega3 l

concatenaOi:: String -> String
concatenaOi x = "oi " ++ x 

saudacao:: [String] -> [String]
saudacao l = map concatenaOi l

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar p [] = []
filtrar p (x:xs) = [l|l<- x:xs, p l]
 
pares :: [Int] -> [Int]
pares l = filter even l 

eq :: Int -> Bool
eq x = (5 * x + 6) < (x * x) 

solucoes :: [Int] -> [Int]
solucoes l = filter eq l

maior :: [Int] -> Int
maior l = foldr1 max l

menor_min10 :: [Int]->Int
menor_min10 l = if foldr max 0 l > 10 then 10 else foldr min 999999 l

junta_silabas_plural :: [String] -> String
junta_silabas_plural [] = []
junta_silabas_plural l = foldr (++) "" l

min10 :: [Int] -> [Int]
min10 [] = []
min10 (x:xs) = if x < 10 then x: min10 xs else min10 xs

menores10 :: [Int] -> ([Int], Int)
menores10 l = (min10 l, length (min10 l))

busca_ind :: Int -> [Int] -> Int
busca_ind n [] = 0
busca_ind n (x:xs) = if n/= x then 1 + busca_ind n xs else 1

busca_elem :: Int -> [Int] -> (Bool, Int)
busca_elem x l = (elem x l, busca_ind x l)