-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat x
  | x < 0 = error "neg szam"
  | otherwise = mod x 10 * szjSzorzat (div x 10)

szjSzorzat2 x
  | x < 0 = error "neg szam"
  | div x 10 == 0 = x
  | otherwise = mod x 10 * szjSzorzat (div x 10)

ls1 = [234, 6, 0, 64321]

szjSzorzatLs ls = map szjSzorzat2 ls

szjSzorzatLs2 = map (\x -> (x, szjSzorzat2 x)) ls1

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg 0 = 0
szjOsszeg x = mod x 10 + szjOsszeg (div x 10)

szjOsszeg2 x
  | x < 0 = error "neg szam"
  | div x 10 == 0 = x
  | otherwise = mod x 10 + szjOsszeg2 (div x 10)

szjOsszegLs ls = map szjOsszeg2 ls

szjOsszegLs2 ls = map (\x -> (x, szjOsszeg2 x)) ls

-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam 0 = 0
szjSzam x = 1 + szjSzam (div x 10)

szjSzam2 x
  | x < 0 = szjSzam2 (abs x)
  | div x 10 == 0 = 1
  | otherwise = 1 + szjSzam2 (div x 10)

szjSzamLs ls = map szjSzam ls

szjSzamLs2 ls = map (\x -> (x, szjSzam2 x)) ls

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
szamSzjOsszeg n szj
  | szj > 9 = error "nem szj."
  | n < 10 = if n == szj then szj else 0
  | otherwise =
      if mod n 10 == szj
        then szj + szamSzjOsszeg (div n 10) szj
        else szamSzjOsszeg (div n 10) szj

szamSzjOsszeg2 n szj elof
  | szj > 9 = error "nem szj."
  | n < 10 = if n == szj then (elof + 1) * szj else elof * szj
  | otherwise =
      if mod n 10 == szj
        then szamSzjOsszeg2 (div n 10) szj (elof + 1)
        else szamSzjOsszeg2 (div n 10) szj elof

ls2 = [(577723707, 7), (54, 7), (0, 0), (124522, 2)]

szamSzjOsszegLs ls = map (uncurry szamSzjOsszeg) ls

szamSzjOsszegLs2 ls = map (\(x, szj) -> (x, szj, szamSzjOsszeg2 x szj 0)) ls

-- - egy szám páros számjegyeinek számát,
parosSzj n
  | n < 0 = parosSzj (abs n)
  | n < 10 = if even n then 1 else 0
  | otherwise = if even utolsoSzj then 1 + parosSzj (div n 10) else parosSzj (div n 10)
  where
    utolsoSzj = mod n 10

parosSzjLs :: [Integer] -> [(Integer, Integer)]
parosSzjLs = map (\x -> (x, parosSzj x))

-- - egy szám legnagyobb számjegyét,
lnSzj n ln
  | n < 0 = lnSzj (abs n) ln
  | n < 10 = max n ln
  | otherwise = if mod n 10 > ln then lnSzj (div n 10) (mod n 10) else lnSzj (div n 10) ln

lnSzjLs ls = map (\x -> (x, lnSzj x (-1))) ls

-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```
bSzamDSzj n b d
  | n < 0 = bSzamDSzj (abs n) b d
  | n < b = if n == d then 1 else 0
  | otherwise = if mod n b == d then 1 + bSzamDSzj (div n b) b d else bSzamDSzj (div n b) b d

ls3 = [(7673573, 10, 7), (1024, 2, 1), (1023, 2, 1), (345281, 16, 4)]

bSzamDSzjLs = map (\(n, b, d) -> bSzamDSzj n b d) ls3

-- - az 1000-ik Fibonacci számot.
fiboN n = fiboSg 0 1 0 n
  where
    fiboSg a b res n
      | n == 0 = res
      | otherwise = fiboSg b res (res + b) (n - 1)

fibo _ _ res 0 = res
fibo a b res n = fibo b res (res + b) (n - 1)

fiboN2 n = fibo 0 1 0 1000

fiboSzamok n = map (fibo 0 1 0) [0 .. n]

fiboSzamok2 n = map (\x -> fibo 0 1 0 x) [0 .. n]

fiboN3 n = fiboSzamok n !! n

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

main :: IO ()
main = do
  let fel1 = szjSzorzat 1234
  print fel1