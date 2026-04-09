-- # 1. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: (Num a) => a -> a -> a
osszeg a b = a + b

osszeg2 :: Int -> Int -> Int
osszeg2 a b = (+) a b

kulonbseg :: (Num a) => a -> a -> a
kulonbseg a b = a - b

kulonbseg2 :: Double -> Double -> Double
kulonbseg2 a b = (-) a b

szorzat :: (Num a) => a -> a -> a
szorzat a b = a * b

szorzat2 :: Double -> Double -> Double
szorzat2 a b = (*) a b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = a `div` b

osztmar :: (Integral a) => a -> a -> a
osztmar a b = mod a b

osztmar2 :: (Integral a) => a -> a -> a
osztmar2 a b = a `mod` b

-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> x = -b / a
elsoF :: (Fractional a) => a -> a -> a
elsoF a b = -(b / a)

-- - egy szám abszulút értékét,
absz a = if a < 0 then -a else a

absz2 a
  | a < 0 = -a
  | otherwise = a

-- - egy szám előjelét,
elojel a = if a < 0 then "neg" else if a > 0 then "poz" else "nulla"

elojel2 a
  | a < 0 = "neg"
  | a > 0 = "poz"
  | otherwise = "nulla"

-- - két argumentuma közül a maximumot,
max1 :: (Ord a) => a -> a -> a
max1 a b = if a > b then a else b

max2 :: (Ord a) => a -> a -> a
max2 a b
  | a > b = a
  | otherwise = b

-- - két argumentuma közül a minimumot,
min1 :: (Ord a) => a -> a -> a
min1 a b = if a < b then a else b

min2 :: (Ord a) => a -> a -> a
min2 a b
  | a < b = a
  | otherwise = b

-- - egy másodfokú egyenlet gyökeit,
-- a*(x**2) + b*x + c = 0 -> a,b,c
-- delta = b**2 - 4*a*c -> delta<0 = error "komplex szamok"
-- gy1 = (-b + sqrt delta) / (2*a)
-- gy2 = (-b - sqrt delta) / (2*a)
masodF a b c = if delta < 0 then error "komplex szamok" else (gy1, gy2)
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b + sqrt delta) / (2 * a)
    gy2 = (-b - sqrt delta) / (2 * a)

masodF2 a b c
  | delta < 0 = error "komplex szamok"
  | otherwise = (gy1, gy2)
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b + sqrt delta) / (2 * a)
    gy2 = (-b - sqrt delta) / (2 * a)

-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
elempar ep1 ep2 = (a == c && b == d) || (a == d && b == c)
  where
    (a, b) = ep1
    (c, d) = ep2

elempar2 (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

-- - az n szám faktoriálisát (3 módszer),
fakt1 0 = 1
fakt1 n = n * fakt1 (n - 1)

fakt2 n
  | n < 0 = error "negativ szam"
  | n == 0 = 1
  | otherwise = n * fakt2 (n - 1)

fakt3 res n
  | n < 0 = error "negativ szam"
  | n == 0 = res
  | otherwise = fakt3 (res * n) (n - 1)

-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).
hatvany1 :: (Ord a, Floating a) => a -> a -> a
hatvany1 x n
  | n < 0 = error "negativ kitevo"
  | otherwise = x ** n

hatvany2 :: (Num a, Integral b) => a -> b -> a
hatvany2 x n
  | n < 0 = error "negativ kitevo"
  | otherwise = x ^ n

hatvany3 x n
  | n < 0 = error "negativ kitevo"
  | n == 0 = 1
  | otherwise = x * hatvany3 x (n - 1)

hatvany4 x n
  | n < 0 = error "negativ kitevo"
  | n == 0 = 1
  | otherwise = x * temp
  where
    temp = hatvany4 x (n - 1)

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgyokN n = [sqrt i | i <- [1 .. n]]

-- - az első n négyzetszámot,
negyzetszamN n = [i * i | i <- [1 .. n]]

-- - az első n természetes szám köbét,
kobN n = [i * i * i | i <- [1 .. n]]

-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzetN n = [i | i <- [1 .. n], i /= sqrt i * sqrt i]

-- - x hatványait adott n-ig,
hatvanyXN x n = [x * i | i <- [0 .. n]]

-- - egy szám páros osztóinak listáját,
parosOsztokX x = [i | i <- [2 .. x], even i, mod x i == 0]

-- - n-ig a prímszámok listáját,
osztok x = [i | i <- [1 .. x], mod x i == 0]

primszam x = osztok x == [1, x]

primszamokN n = [i | i <- [1 .. n], primszam i]

primszamokN2 n = [i | i <- [1 .. n], primszamL i]
  where
    primszamL x = osztokL x == [1, x]
    osztokL x = [i | i <- [1 .. n], mod x i == 0]

-- - n-ig az összetett számok listáját,
osszetettN n = [i | i <- [1 .. n], not (primszam i)]

-- - n-ig a páratlan összetett számok listáját,
oddOsszetettN n = [i | i <- [1 .. n], odd i, not (primszam i)]

-- - az n-nél kisebb Pitágorászi számhármasokat,
pitN n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], (a ^ 2) + (b ^ 2) == c ^ 2]

-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a' .. 'z'] [0 .. 25]

-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok = zip [0 .. 5] [5, 4 .. 0]

szamok2 n = [(i, n - i) | i <- [0 .. n]]

-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
tfLs n = take n ls
  where
    ls = [True, False] ++ ls

-- ** Megoldott feladatok:**

-- - Határozzuk meg egy szám osztóinak listáját:

--   ```haskell
--   osztok :: Int -> [ Int ]
--   osztok n = [ i | i <- [1..n] , n `mod` i ==0]

--   > osztok 100
--   ```

-- - Határozzuk meg a következő listát: $$[(\texttt{a},0), (\texttt{b},1), \ldots, (\texttt{z}, 25)]$$:

--   ```haskell
--   import Data.Char
--   lista = [(chr(i + 97), i) | i<-[0..25]]

--   lista_ = zip ['a'..'z'] [1..26]
--   ```
