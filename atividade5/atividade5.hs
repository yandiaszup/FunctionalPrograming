
--1--
conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x:xs) = 1 + conta_ch xs

conta :: [t] -> Int
conta [] = 0
conta (_:xs) = 1+conta xs

maior :: [Int] -> Int
maior [x] = x
maior (x:y:xs)
 |x>y = maior (x: xs)
 |otherwise = maior (y:xs)
 
primeiros :: Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x: primeiros (n-1) xs

pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = (a == x) || pertence a z

uniaoR :: Eq t => [t] -> [t] -> [t]
uniaoR [] l = l
uniaoR (x:xs) l = if pertence x l then uniaoR xs l else x: uniaoR xs l

npares :: [Int] -> Int
npares [] = 0
npares (x:xs) = if even x then 1 + npares xs else npares xs

produtorio :: [Int] -> Int
produtorio [x] = x 
produtorio (x:xs) = x * produtorio xs

comprime :: [[a]] -> [a]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

tamanho :: [t] -> Int
tamanho [] = 0
tamanho (_:r) = 1+conta r

uniaoRec2 :: Eq t => [t] -> [t] -> [t]
uniaoRec2 [] l = l
uniaoRec2 l (x:xs) = if pertence x l then uniaoR xs l else x: uniaoR xs l