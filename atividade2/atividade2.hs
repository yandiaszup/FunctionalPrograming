type Data = (Int,Int,Int)
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

dobro :: Float -> Float
dobro x = x * 2.0

quadruplo :: Float -> Float
quadruplo x = dobro(dobro x)

hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt(x^2 + y^2)

calculaDistancia :: (Float, Float) -> (Float, Float) -> Float
calculaDistancia (x,y) (a,b) = hipotenusa (abs(x - a)) (abs(y - b)) 
 
conversao :: Float -> (Float,Float,Float)
conversao a = (a , a * 3.96 , a * 4.45)

bissexto :: Int -> Bool
bissexto x | mod x 400 == 0 = True
 | (mod x 4 == 0) && (mod x 100 /= 0) = True
 | otherwise = False
 
bissexto2 :: Data -> Bool
bissexto2 (d,m,y)
 | d >= 1 && d <= 31 && m >= 1 && m <= 12 && bissexto y = True
 | otherwise = False

valida :: Data -> Bool
valida (d,m,y)
 | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 ||
 m == 7 || m == 8 || m == 10 || m == 12) = True
 | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 ||
 m == 11) = True
 | d >= 1 && d <= 28 && m == 2 && not (bissexto y) = True
 | d >= 1 && d <= 29 && m == 2 && bissexto y = True
 
precede :: Data -> Data -> Bool
precede (d1,m1,y1) (d2,m2,y2) 
 |y1 <= y2 && m1 <= m2 && d1 <= d2 = True
 |otherwise = False
 
e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")
emDia :: Emprestimo -> Data ->Bool
emDia (_,_,_,d1,_) d2
 |precede d1 d2 = True
 |otherwise = False