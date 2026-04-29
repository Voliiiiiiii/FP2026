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



--Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.


mySplitAt 0 xs     = ([], xs)
mySplitAt _ []     = ([], [])
mySplitAt n (x:xs) = (x:elso, masodik)
    where (elso, masodik) = mySplitAt (n-1) xs



rovidszavak ls =  filter (\x -> length x == hossz) ls
    where hoszlista = map length ls 
          hossz = foldr1 min hoszlista
    



--Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
-- Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.
-- > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
-- [2, 6, 8]
-- > talalat 'e' "Bigeri-vizeses"
-- [3,10,12]


talalat x ls =  map (\(i,e) -> i)  $ filter (\(i,e) -> e == x )  (zip [0..] ls)

-- Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
-- Például:
-- > ls = [("golya",120),("fecske",85),("cinege",132)]
-- > osszegT ls
-- 337


osszegT ls = foldl (\res (_, x) -> res + x) 0 ls 

--Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. 
--  Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket. Például:

-- > :set +m
-- > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
-- > atlagTu ls
-- mari 7.375
-- feri 9.0
-- zsuzsa 7.466666666666666
-- levi 8.875


atlagTu ls = map (\(x, ls_inside) -> (x, sum ls_inside / fromIntegral (length ls_inside))) ls
          


