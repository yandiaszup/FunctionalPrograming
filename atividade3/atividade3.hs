(||) :: Bool -> Bool -> Bool
True || False = True
True || True = True
a || False = a

_ || _ = True
False || False = False

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt(a^2 + b^2)

calculaDistancia :: (Float, Float) -> (Float, Float) -> Float
calculaDistancia (a,b) (a1,b1) = hipotenusa (abs(a - a1)) (abs(b - b1)) 

fatorial :: Int -> Int
fatorial 0 = 1
fatorial a = a * fatorial (a - 1)

fatorial2 :: Int -> Int
fatorial2 n
 |n ==0 = 1
 |otherwise = n * fatorial2 (n - 1)

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo2(n-1)

fibo2 :: Int -> Int
fibo2 n
 |n == 1 = 1
 |n == 2 = 1
 |otherwise = fibo2(-2) + fibo2(n - 1)

n_tri :: Int -> Int
n_tri 0 = 0
n_tri n = n_tri (n - 1) + n

potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 a = potencia2 (a - 1) * 2

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n = if m > n then 0 else if m == n then m else (prodIntervalo (m + 1) n) * m


resto_div :: Int -> Int -> Int
resto_div m n = if m-n < n then m - n else (resto_div (m - n) n)

divInteira :: Int -> Int -> Int
divInteira m n = if m-n > n then (divInteira (m - n) n) + 1 else 1

mdc :: (Int, Int) -> Int
mdc (m, n) 
 |n == 0 = m
 |otherwise = mdc(n, (mod m n))
 
mdc2 :: (Int, Int) -> Int
mdc2 (m, 0) = m
mdc2 (m, n) = mdc (n, (mod m n))

binomialg :: (Int,Int) -> Int
binomialg (n,0) = 1
binomialg (n,k)
 |k == 0 = 1
 |k == n = 1
 |otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)
 
binomial :: (Int, Int) -> Int
binomial (n, 0) = 1
binomial (n, k) = if (k == n) then 1 else binomial (n-1, k) + binomial (n-1, k-1)

passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (y, x + y)

fibo_par :: Int -> (Int, Int)
fibo_par 0 = (0, 1)
fibo_par n = passo(fibo_par (n - 1)) 

first :: (Int, Int) -> Int
first (x, y) = x

fib2 :: Int -> Int
fib2 x = first (fibo_par x) 