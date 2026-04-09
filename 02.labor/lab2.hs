-- # 2. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat n = mod n 10 * szjSzorzat (div n 10)

szjSzorzat2 n
  | n < 10 = n
  | n == 0 = 1
  | otherwise = mod n 10 * szjSzorzat2 (div n 10)

szorzatLs ls = map szjSzorzat2 ls

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg 0 = 0
szjOsszeg n = mod n 10 + szjOsszeg (div n 10)

szjOsszeg2 n
  | n < 10 = n
  | n == 0 = 0
  | otherwise = mod n 10 + szjOsszeg2 (div n 10)

osszegLs ls = map szjOsszeg ls

-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam 0 = 0
szjSzam n = 1 + szjSzam (div n 10)

szjSzam2 n
  | n < 10 = n
  | n == 0 = 0
  | otherwise = 1 + szjSzam2 (div n 10)

szjSzamLs ls = map szjSzam ls

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:
--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
szSzjOsszeg 0 szj = 0
szSzjOsszeg n szj = if mod n 10 == szj then szj + szSzjOsszeg (div n 10) szj else szSzjOsszeg (div n 10) szj

szSzjOsszeg2 n szj
  | n == 0 = 0
  | mod n 10 == szj = szj + szSzjOsszeg2 (div n 10) szj
  | otherwise = szSzjOsszeg2 (div n 10) szj

szSzjOsszegLs ls = map (uncurry szSzjOsszeg) ls

-- - egy szám páros számjegyeinek számát,
szParosSzj 0 = 0
szParosSzj n = if even utolso_szj then 1 + szParosSzj (div n 10) else szParosSzj (div n 10)
  where
    utolso_szj = mod n 10

szParosSzj2 n
  | n == 0 = 0
  | even (mod n 10) = 1 + szParosSzj2 (div n 10)
  | otherwise = szParosSzj2 (div n 10)

szParosSzjLs ls = map szParosSzj ls

-- - egy szám legnagyobb számjegyét,
-- pl. meghivas: lgSzj 1232345872349234 0
lgSzj 0 maxSzj = maxSzj
lgSzj n maxSzj = lgSzj (div n 10) (max utolsoSzj maxSzj)
  where
    utolsoSzj = mod n 10

lsSzjLs ls = map (uncurry lgSzj) ls

-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:
--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```

bSzrDSzj 0 b d = 0
bSzrDSzj n b d
  | utszj == d = 1 + bSzrDSzj (div n b) b d
  | otherwise = bSzrDSzj (div n b) b d
  where
    utszj = mod n b

bdLs = [(7673573, 10, 7), (1024, 2, 1), (1023, 2, 1), (345281, 16, 4)]

bSzrDSzjLs = map (\(n, b, d) -> bSzrDSzj n b d) bdLs

-- - az 1000-ik Fibonacci számot.
fibo a b res n
  | n == 0 = res
  | otherwise = fibo b res (b + res) (n - 1)

fiboN n = fibo 0 1 1 n

fiboN2 n = fiboL 0 1 1 n
  where
    fiboL a b res n
      | n == 0 = res
      | otherwise = fibo b res (res + b) (n - 1)

fiboLs ls = map fiboN2 ls

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- ** Megoldott feladatok:**

-- - Határozzuk meg egy szám számjegyeinek összegét:
--   I. módszer:

--   ```haskell
--   szOsszeg :: Int -> Int
--   szOsszeg 0 = 0
--   szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--   > szOsszeg 123
--   ```

--   II. módszer:

--   ```haskell
--   szOsszeg1 :: Int -> Int -> Int
--   szOsszeg1 0 t = t
--   szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--   > szOsszeg1 123 0
--   ```
