-- labor   öt

type Pont = (Double, Double)

lsP :: [Pont]
lsP = [(4.5,6.2),(1.2,3.4),(6,8),(4.5,2.4)]

p :: Pont
p = (3.4,5.6)

tavolsag (x1,y1) (x2,y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

minTavolsaf lsP p =
    foldl1 (\p1 p2 -> if tavolsag p1 p < tavolsag p2 p then p1 else p2) lsP



parosNegyzet2 n = take n [i ^ 2 | i <- [0,2 ..]]

lsN n = [n,n-1 .. 1] ++ [1 .. n]

tf n = [even i | i <- [0 .. n]]

osztok n  =[i | i <- [ 1 .. n], mod n i == 0]

lnP n = maximum [ i | i <- osztok n, odd i]

lnP2 n = foldl1 (\res i -> if mod n i == 0 then i else res)

fibo = fiboSg 0 1 0
    where 
        fiboSg a b res = res : fiboSg b res (res + b)
    

listaNelem2 ls n i 
    | i >= length ls = []
    | mod i n == 0 = ls !! i : listaNelem2 ls n (i + 1)
    | otherwise = listaNelem2 ls n (i + 1)

