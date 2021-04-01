intervalo :: Int -> Int -> [Int]
intervalo a b 
 |a == b = []
 |otherwise = [x|x<-[a..b]]
 
intervaloPar :: Int -> Int -> [Int]
intervaloPar a b 
 |a == b = []
 |otherwise = [x | x <- [a..b], even x]
 
quadrados :: Int -> Int -> [Int]
quadrados a b = [x^2 | x <- [a..b]]

selecionaImpares :: [Int] -> [Int]
selecionaImpares x = [n | n <- x, odd n]

tabuada :: Int -> [Int]
tabuada x =[y*x | y <- [1..10]] 

bissexto :: Int -> Bool
bissexto x 
    |(mod x 4 == 0 && not(mod x 100==0))||(mod x 400==0)=True
    |otherwise = False 

bissextos :: [Int] -> [Int]
bissextos = filter bissexto

bissexto2 :: Data -> Bool
bissexto2 (_,_,ano) = bissexto ano
sublistas x = concat x

type Data = (Int , Int , Int)
valida :: Data ->Bool
valida (d,m,y) 
    |d>=1 && d<=31 && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True
    |d>=1 && d<=30 && (m==4 || m==6 || m==9 || m==11) = True
    |d>=1 && d<=28 &&  m==2 && not(bissexto2 (d,m,y)) = True
    |d>=1 && d<=29 &&  m==2 && bissexto2 (d,m,y) = True
    |otherwise = False
  
precede :: Data -> Data -> Bool
precede (d1,m1,y1) (d2,m2,y2)
    |(valida (d1,m1,y1) && valida(d2,m2,y2))&&(y1<y2) = True
    |(valida (d1,m1,y1) && valida(d2,m2,y2))&&((y1==y2) && (m1<m2)) = True
    |(valida (d1,m1,y1) && valida(d2,m2,y2))&&((y1==y2) && (m1==m2) && (d1<d2)) = True
    |otherwise = False


type Livro = (String , String , String , Int)
type Aluno = (String , String , String , Int)
type Emprestimo = (String , String , Data , Data , String )
type Emprestimos = [Emprestimo]

checaEmprestimo :: Emprestimo -> Data -> Bool
checaEmprestimo (_,_,_,data_devolucao,"aberto") data_hoje
    | precede data_hoje data_devolucao = True
    | otherwise = False

checaEmprestimo (_,_,_,data_devolucao,"encerrado") data_hoje 
    | precede data_hoje data_devolucao = False
    | otherwise = True

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados x data_hoje = [y|y<-x, checaEmprestimo y data_hoje]


bdEmprestimo :: Emprestimos
bdEmprestimo = [   ("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
                    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
                    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = a == x || pertence a z

uniaoNRec:: Eq t => [t] -> [t] -> [t]
uniaoNRec [] l = l
uniaoNRec (x:xz) l = if pertence x l then uniaoNRec xz l
else x: uniaoNRec xz l