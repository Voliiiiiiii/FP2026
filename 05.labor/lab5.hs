-- # 5. labor
import Debug.Trace

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.
splitAt' idx ls = (idxElott, idxUtan)
  where
    idxElott = take idx ls
    idxUtan = drop idx ls

notElem' elem [] = True
notElem' elem (x : xs)
  | elem == x = False
  | otherwise = notElem' elem xs

concat' [] = []
concat' (x : xs) = x ++ concat' xs

repeat' x = x : repeat' x

replicate' db x
  | db == 0 = [x]
  | otherwise = x : replicate (db - 1) x

replicate2 db x = take db $ repeat' x

cycle' ls = ls ++ cycle' ls

iterate' f kezdoErtek = kezdoErtek : iterate' f (f kezdoErtek)

any' f [] = False
any' f (x : xs)
  | f x = True
  | otherwise = any' f xs

all' f [] = True
all' f (x : xs)
  | not $ f x = False
  | otherwise = all' f xs

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
length' ls = foldl (\db _ -> 1 + db) 0 ls

sum' ls = foldl (\acc x -> acc + x) 0 ls

sum2 ls = foldl1 (+) ls

elem' x ls = foldl (\acc e -> if e == x then True else acc) False ls

reverse' ls = foldr (\x acc -> acc ++ [x]) [] ls

reverse2 ls = foldl (\acc x -> x : acc) [] ls

product' ls = foldl (*) 1 ls

product2 ls = foldl1 (*) ls

maximum' ls = foldl1 (max) ls

maximum2 ls = foldl1 aux ls
  where
    aux x1 x2
      | x1 > x2 = x1
      | otherwise = x2

insertSort' ls = foldr insert [] ls
  where
    insert x [] = [x]
    insert x (y : ve)
      | x <= y = x : y : ve
      | otherwise = y : insert x ve

insertSort2 ls = foldr insert [] ls
  where
    insert x [] = trace ("insert " ++ show x ++ " into []") [x]
    insert x (y : ve)
      | x <= y =
          trace ("insert " ++ show x ++ " before " ++ show y ++ "(" ++ show ve ++ ")") (x : y : ve)
      | otherwise =
          trace ("keep " ++ show y ++ ", recurse" ++ "(" ++ show ve ++ ")") (y : insert x ve)

lsFuz ls = foldl (\acc x -> acc ++ x) [] ls

map' f ls = foldl (\acc x -> acc ++ [f x]) [] ls

filter' f ls = foldl (\acc x -> if f x then acc ++ [x] else acc) [] ls

-- - meghatározza egy lista pozitív elemeinek összegét,
pozElemOsszeg ls = foldl (\acc x -> if x > 0 then acc + x else acc) 0 ls

-- - egy lista páros elemeinek szorzatát,
parosElemSzorzat ls = foldl (\acc x -> if even x then acc * x else acc) 1 ls

-- - n-ig a négyzetszámokat.
negyzetN n = foldr (\x acc -> x * x : acc) [] [1 .. n]

-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$
poli x aLs = foldr (\e acc -> e + (x * acc)) 0 aLs

-- III.

{- Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat,
amelyek karakterszáma a legkisebb.
Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion
akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe -}
ls = ["function", "class", "Float", "higher-order", "monad", "tuple", "variable", "Maybe", "recursion"]

minString ls = filter (\x -> length x == minLength) ls
  where
    minLength = length $ minimum ls

-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
talalat x ls = [i | (e, i) <- zip ls [0 ..], e == x]

-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
ls1 = [("golya", 120), ("fecske", 85), ("cinege", 132)]

osszegT ls = foldl (\acc (s, n) -> acc + n) 0 ls

osszegT2 ls = sum $ map snd ls

-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```

ls2 = [("mari", [10, 6, 5.5, 8]), ("feri", [8.5, 9.5])]

atlag ls = sum ls / fromIntegral (length ls)

atlagTu ls = mapM_ (\(nev, jegyek) -> putStrLn (nev ++ " " ++ show (atlag jegyek))) ls
