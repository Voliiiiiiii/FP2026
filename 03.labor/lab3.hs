import Data.Foldable
import Data.Function
import Data.List

-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2] -- prefix modon jeloljuk az elvegzendo muveleteket, a . altal egy uj fuggveny jon letre, ami eloszor a filtert majd az atlagot vegzi el
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2] -- nem hozunk letre uj fuggvenyt, a $ (function application) jellel jeloljuk az osszefuzest
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
myLength [] = 0
myLength (x : ls) = 1 + myLength ls

myLength2 [] res = res
myLength2 (x : ls) res = myLength2 ls (res + 1)

myLength3 ls = foldr (\db x -> (+) db 1) 0 ls

myLength4 ls = foldl (\x db -> (+) 1 db) 0 ls

ls1 = [[1, 23, 56, 78], []]

ls2 = [[1, 23, 56, 78], [12, 546]]

myLengthLs ls = map myLength ls

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
myProduct [] = 1
myProduct (x : ls) = x * myProduct ls

myProduct2 [] res = res
myProduct2 (x : ls) res = myProduct2 ls (res * x)

myProduct3 ls = foldr (*) 1 ls

myProduct4 ls = foldl (\x db -> x * db) 1 ls

myProductLs ls = map myProduct ls

-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum [] = error "ures lista"
myMinimum [x] = x
myMinimum (x1 : x2 : ls)
  | x1 < x2 = myMinimum (x1 : ls)
  | otherwise = myMinimum (x2 : ls)

myMinimum2 [] = error "ures lista"
myMinimum2 [x] = x
myMinimum2 (x : ls) = min x (myMinimum2 ls)

myMinimum3 ls = foldl1 min ls

myMinimum4 ls = minimum ls

myMinimumLs ls = map myMinimum ls

-- - meghatározza egy lista legnagyobb elemét (myMaximum),
myMaximum [] = error "ures lista"
myMaximum [x] = x
myMaximum (x1 : x2 : ls)
  | x1 > x2 = myMaximum (x1 : ls)
  | otherwise = myMaximum (x2 : ls)

myMaximum2 [] = error "ures lista"
myMaximum2 [x] = x
myMaximum2 (x : ls) = max x (myMaximum2 ls)

myMaximum3 ls = maximum ls

myMaximum4 ls = foldl1 max ls

myMaximumLs ls = map myMaximum ls

-- - meghatározza egy lista n-ik elemét (!!),
nElem ls n
  | null ls = error "ures lista"
  | n < 0 = error "neg. index"
  | length ls < n = error "tul nagy index"
  | otherwise = ls !! n

nElem2 ls n = ls !! n

-- ls3 :: [([Integer], Int)]
ls3 = [([123, 532, 6], 1), ([456, 89], 0), ([1], 0)]

nElemLs = map (uncurry nElem) ls3

-- - egymásután fűzi a paraméterként megadott két listát (++),
fuzLs ls1 ls2 = ls1 ++ ls2

fuzLs2 ls1 ls2 = (++) ls1 ls2

fuzLs3 ls1 ls2
  | null ls1 && null ls2 = error "ures listak"
  | null ls1 = ls2
  | null ls2 = ls1
  | otherwise = ls1 ++ ls2

fuzLsMap ls1 ls2 = map (uncurry fuzLs) (zip ls1 ls2)

-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
palindrom ls = ls == reverse ls

palindrom2 [] = True
palindrom2 [_] = True
palindrom2 ls = ls == reverse ls

palindrom3 [] = True
palindrom3 [_] = True
palindrom3 ls = (head ls == last ls) && palindrom3 (init $ tail ls)

palindromLs ls = map palindrom ls

-- - meghatározza egy egész szám számjegyeinek listáját,
szjLs x
  | x == 0 = [0]
  | div x 10 == 0 = [x]
  | otherwise = szjLs (div x 10) ++ [mod x 10]

szjLs2 x
  | x == 0 = [0]
  | div x 10 == 0 = [x]
  | otherwise = mod x 10 : szjLs2 (div x 10)

szjLsMap ls = map szjLs ls

-- - a lista első elemét elköltözteti a lista végére,
elsoUtolso [] = error "ures lista"
elsoUtolso [k] = [k]
elsoUtolso (k : ve) = ve ++ [k]

elsoUtolso2 ls = tail ls ++ [head ls]

elsoUtolsoLs ls = map elsoUtolso ls

-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
atlagLs :: (Fractional a) => [a] -> a
atlagLs [] = error "ures lista"
atlagLs [e] = e
atlagLs ls = sum ls / fromIntegral (length ls)

ls4 = [[1, 2, 3], [4, 5, 6]]

atlagLsMap = map atlagLs ls4

-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
decToP x p
  | x == 0 = [0]
  | x < p = [x]
  | otherwise = decToP (div x p) p ++ [mod x p]

decToP2 x p = aux x []
  where
    aux 0 ls = ls
    aux x ls =
      let (hatvany, maradek) = divMod x p
       in aux hatvany (maradek : ls)

ls5 = [(2, 2), (1024, 2)]

decToPLs = map (uncurry decToP) ls5

-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.
pToDec ls p = foldl (\acc x -> acc * p + x) 0 ls

pToDec2 x p = [szj * (p ^ hatvany) | (szj, hatvany) <- zip (szamjegyek x p) [0 ..]]
  where
    szamjegyek 0 p1 = []
    szamjegyek x1 p1 = mod x1 p1 : szamjegyek (div x1 10) p1

ls6 = [(011, 2), (100, 2)]

pToDec2Ls = map (uncurry pToDec2) ls6

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.
-- Horner elrendezes -> P(x) = (...(an*x+an-1)*x+...)*x+a0

aLs = [3, -2, 5, -7]

x0 = 2

poli [] x = 0
poli (a : aLs) x = a + x * poli aLs x

poli2 aLs x = seged aLs x 1 0
  where
    seged [] _ _ eredmeny = eredmeny
    seged (a : aLs) x hatvany eredmeny = seged aLs x (hatvany * x) (eredmeny + a * hatvany)

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
type Pont = (Double, Double)

pont :: Pont
pont = (5.6, 2.3)

lsP :: [Pont]
lsP = [(0, 0), (2.5, 7), (5.2, 2), (1, 87)]

tavolsag :: (Floating a) => (a, a) -> (a, a) -> a
tavolsag (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

legkozelebbiPont :: (Foldable t, Ord b, Floating b) => (b, b) -> t (b, b) -> (b, b)
legkozelebbiPont p ls = minimumBy (compare `on` tavolsag p) ls

legkozelebbiPont2 p ls = foldl1 minPont ls
  where
    minPont p1 p2 = if tavolsag p p1 <= tavolsag p p2 then p1 else p2

main :: IO ()
main = do
  putStr ("legkisebb tavolsagra levo pont " ++ show pont ++ " es ")
  putStr $ intercalate ", " $ map show lsP
  putStrLn " pontok kozott"
  print $ legkozelebbiPont pont lsP