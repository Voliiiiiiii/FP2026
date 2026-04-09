import Data.List
import Data.Ord

--  Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- az első n páros szám négyzetét,
printLs ls = mapM_ (\e -> putStr (show e ++ " ")) ls

parosNegyzet n = [i * i | i <- [0 .. n], even i]

parosNegyzet2 n = take n [i * i | i <- [0, 2 ..]]

parosNegyzet3 n = take n [(i, i * i) | i <- [0, 2 ..]]

parosNegyzet4 n = mapM_ print res
  where
    res = [(i, i ** 2) | i <- [0, 2 .. n]]

parosNegyzet5 n = mapM_ myPrint res
  where
    res = [(i, i ** 2) | i <- [0, 2 .. n]]
    myPrint (szam, negyzet) = putStrLn (show szam ++ " negyzete " ++ show negyzet)

-- az első [1, 2, 2, 3, 3, 3, 4, 4, 4, 4,...],
szamokLs n = take n $ szamok 1
  where
    szamok i = aux i ++ szamok (i + 1)
    aux i = replicate i i

szamokLs2 n i
  | i /= n = replicate i i ++ szamokLs2 n (i + 1)
  | otherwise = replicate i i

-- az első [2, 4, 4, 6, 6, 6, 8, 8, 8, 8...],
szamokLs3 n i
  | i /= n = replicate i (2 * i) ++ szamokLs3 n (i + 1)
  | otherwise = replicate i (2 * i)

-- az első [n, n-1, ... 2, 1, 1, 2, ..., n-1, n],
szamokLs4 n = reverse [1 .. n] ++ [1 .. n]

szamokLs5 n = [n, n - 1 .. 1] ++ [1 .. n]

-- váltakozva tartalmazzon True és False értékeket,
trueFalseLs n = take n lista
  where
    lista = [True, False] ++ lista

-- váltakozva tartalmazza a 0, 1, -1 értékeket.
szamokLs6 n = take n ls
  where
    ls = [0, 1, -1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- meghatározza egy adott szám osztóinak számát,
osztok x = [i | i <- [1 .. x], mod x i == 0]

osztokSz x = length (osztok x)

osztokSz2 x = foldr (\_ db -> 1 + db) 0 (osztok x)

osztokSz3 x = myLength (osztok x)
  where
    myLength [] = 0
    myLength (x : xs) = 1 + myLength xs

-- meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlanOszto x = last $ filter odd $ osztok x

maxParatlanOszto2 x = maximum $ filter odd $ osztok x

-- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decToP x p
  | x < p = [x]
  | otherwise = decToP (div x p) p ++ [mod x p]

decToPSzj x p = length (decToP x p)

-- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
decToPMax x p = maximum $ decToP x p

-- meghatározza az a és b közötti Fibonacci számokat, a > 50 .
fiboN n = fibo 0 1 0 n
  where
    fibo _ _ res 0 = res
    fibo n1 n2 res n = fibo n2 res (res + n2) (n - 1)

fiboN2 n = fibo n 0 1
  where
    fibo 0 a b = [a]
    fibo n a b = a : fibo (n - 1) b (a + b)

fiboN3 n = take n (fibo 0 1)
  where
    fibo a b = a : fibo b (a + b)

fibo = fiboS 0 1 0
  where
    fiboS a b res = res : fiboS b res (b + res)

fiboAB a b = dropWhile (< a) $ takeWhile (< b) (fibo)

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely
-- meghatározza egy lista pozitív elemeinek átlagát,
lsPozAtlag ls = sum ls1 / fromIntegral (length ls1)
  where
    ls1 = filter (> 0) ls

lsPozAtlag2 ls = osszeg / db
  where
    osszeg = sum [i | i <- ls, i > 0]
    db = fromIntegral (length ls)

-- meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
lsN ls n =
  case drop (n - 1) ls of
    (k : ve) -> k : lsN ve n
    [] -> []

lsN2 ls n = [snd i | i <- zip [1 ..] ls, mod (fst i) n == 0]

-- tükrözi egy lista elemeit,
tukroz [] = []
tukroz (x : xs) = tukroz xs ++ [x]

tukroz2 ls = reverse ls

-- két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
maxElemI ls = ids
  where
    maxElem = maximum ls
    ids = [i | (i, elem) <- zip [1 ..] ls, elem == maxElem]

maxElemI2 ls =
  let maxElem = maximum ls
      ids = [i | (i, elem) <- zip [1 ..] ls, elem == maxElem]
   in ids

maxElemI3 [] = []
maxElemI3 (x : xs) = reverse positions
  where
    (_, positions) = foldl update (x, [0]) (zip xs [1 ..])
    update (currentMax, positions) (elem, idx)
      | elem > currentMax = (elem, [idx])
      | elem == currentMax = (currentMax, idx : positions)
      | otherwise = (currentMax, positions)

maxElemI4 ls = getMaxi 1 maxElem ls []
  where
    maxElem = maximum ls
    getMaxi _ _ [] res = res
    getMaxi i maxe (k : ve) res
      | k == maxe = getMaxi (i + 1) maxe ve (res ++ [i])
      | otherwise = getMaxi (i + 1) maxe ve res

-- meghatározza egy lista leggyakrabban előforduló elemét.
leggyakoribb [] = []
leggyakoribb ls = k : leggyakoribb ve
  where
    k = (kurrens, length [e | e <- ls, e == kurrens])
    ve = [e | e <- ls, e /= kurrens]
    kurrens = head ls

leggyakoribb2 [] = error "ures lista"
leggyakoribb2 ls = head $ maximumBy (comparing length) $ group $ sort ls

leggyakoribb3 ls = head lgyE
  where
    elof = leggyakoribb ls
    lgyE = sortOn (Down . snd) elof
  