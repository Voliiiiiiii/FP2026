-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat n = mod n 10 * szjSzorzat (div n 10)

szjSzorzat2 n
  | n < 0 = error "neg. szam"
  | div n 10 == 0 = n
  | otherwise = mod n 10 * szjSzorzat2 (div n 10)

szjSzorzat3 n res
  | n < 0 = error "neg. szam"
  | div n 10 == 0 = res * n
  | otherwise = szjSzorzat3 (div n 10) (res * (mod n 10))

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg::Integral t => t -> t
szjOsszeg n
  | n < 0 = szjOsszeg (abs n)
  | n < 10 = n
  | otherwise = mod n 10 + szjOsszeg (div n 10)


-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam::(Num t, Integral a) => a -> t -> t
szjSzam n res
  | n < 0 = szjSzam (abs n) res
  | n < 10 = res + 1
  | otherwise = szjSzam (div n 10) (res + 1)



-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:
szjSzamOsszeg n szj
  | szj > 9 = error "nem szamj."
  | n < 10 = if n == szj then szj else 0
  | otherwise = if mod n 10 == szj then szj + szjSzamOsszeg (div n 10) szj else szjSzamOsszeg(div n 10) szj



--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
-- - egy szám páros számjegyeinek számát,
parosSzamSzj2 n res
  | n < 0 = parosSzamSzj2 (abs n) res
  | n < 10 = if even n then res +1 else res
  | otherwise =
      if even (mod n 10)
        then parosSzamSzj2 (div n 10) (res +1)
        else parosSzamSzj2 (div n 10) res 


-- - egy szám legnagyobb számjegyét,

lgSzj n ln
  | n < 0 = lgSzj(abs n) ln
  | n < 10 = if n > ln then n else ln
  | otherwise = 
    if mod n 10 > ln
      then lgSzj(div n 10) (mod n 10)
      else lgSzj(div n 10) ln

      
-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```
-- - az 1000-ik Fibonacci számot.

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.