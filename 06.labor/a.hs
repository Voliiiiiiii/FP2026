import Distribution.SPDX (LicenseId(VOSTROM))
import Language.Haskell.TH (prim, parS)
--I. Írjunk egy-egy Haskell függvényt, amely beolvass a billentyűzetről két természetes számot és kiírja a képernyőre

import Numeric(showIntAtBase)
import Data.List (sort)
import Control.Monad.RWS (MonadState(put))

osztok x = [i | i <- [1 .. x], mod x i == 0]
primszam x = [1, x] == osztok x

main_I = do
    putStr "x1="
    x1 <- readLn :: IO Int

    putStr "x2="
    x2 <- readLn :: IO Int

    putStrLn ("x1=" ++ show x1 ++ ", x2=" ++ show x2)
    let ls = if x1 < x2 then [x1 .. x2] else [x2 .. x1]
    putStrLn "Lista elemei: "
    print ls
    
    --a két szám közötti számok összegét,
    let osszeg = sum ls
    putStrLn ("Lista elemeinek osszege: " ++ show osszeg)
    --a két szám közötti prímszámok összegét,

    let primosszeg= sum $ filter primszam $ ls
    putStrLn ("Primszamok osszege: " ++ show primosszeg)
    
    
    --a két szám közötti azon számokat, amelyeknek legtöbb valódi osztója van.
    let legtobb_valodi_oszto = legtobbVO ls
    putStrLn ("Legtobb valodi oszto szamok: " )
    print legtobb_valodi_oszto


legtobbVO ls =  map (\(_,i) -> i) $ filter (\(o,x) -> o == maxl) osztokszama
    where osztokhosz = map length $ map osztok ls
          osztokszama = zip  osztokhosz ls
          maxl = maximum osztokhosz




fiboSg a b res = res : fiboSg b res (res + b)
--II. Írjunk egy-egy Haskell függvényt, amely beolvassa a billentyűzetről az n természetes számot és kiírja a képernyőre
fibo n = takeWhile (< n) $ fiboSg 0 1 0


primlista n = takeWhile (<n) [x | x <- [2..], primszam x]

main_II = do
    putStr "n="
    n <- readLn :: IO Int
    putStrLn ("N  ====== " ++ show n)

    
--n-ig a Fibonacci számok listáját (n> 50 ), úgy hogy a számok közé szóközt ír,

    let fiboLs = fibo n
    print fiboLs

    putStrLn "fiboszamok (mapM_-el)"
    mapM_ (\x -> putStr (show x ++ " ")) fiboLs

--n-ig a prímszámok listáját, úgy hogy a számok közé szóközt ír,
    putStrLn "\nprimszaok (unwords-el)"
    let primLS = primlista n
    putStrLn $ unwords $ map show primLS


to_binary n = reverse $ vegso n 
    where 
        vegso 0 = ""
        vegso x = show (mod x 4) ++ vegso (div x 4)
    

--  az n kettes számrendszerbeli alakját, úgy hogy minden negyedik bit után egy szóközt ír,
    

-- az n 16-os számrendszerbeli alakját, úgy hogy minden két szimbólum után egy szóközt ír, illetve az a, b, c, d, e, f szimbólumokat használja a 10-nél nagyobb számjegyek kódolására,

-- az n értékének megfelelően a következő sorokat:






fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 2. Az n-edik Fibonacci szám kinyerése a listából
-- Az (!!) operátorral egyszerűen kiemeljük az n. sorszámú elemet
fibon n = fibs !! n

-- A többi részed maradhat majdnem ugyanaz
fibonaci_szorszam ls = map (\x -> (x, fibon x)) ls 

tetel_1_1 = do
    let ls = [10, 25, 1000, 0, 15, 5000]  
    putStrLn "A lista elemei: "
    let ls2 = fibonaci_szorszam ls
    -- A mapM_ itt tökéletes a soronkénti kiíráshoz
    mapM_ (\(i,num) -> putStrLn $ show i ++ " : " ++ show num) ls2


varos_filter n ls = filter (\(_,szam) -> szam > n) ls
tetel_1_2 = do
    let n = 150000 
    let ls = [("sepsiszentgyorgy",54000),("kolozsvár",330000),("marosvasarhely",130000),("temesvar",310000),("arad",160000),("gyergyoszentmiklos",18000),("nagyvarad",196000)]
    let filteredls = sort $ varos_filter n ls

    mapM_ (\(varos,_) -> putStrLn $ varos ++ "" ) filteredls
    

zero_filter num 
    | num == 0 = False
    | num < 10 = True  
    | mod num 10 == 0 = False
    | otherwise  = zero_filter (div num 10)
    
haloka n = take n $  cycle ["IGEN","NEM","TARTOZKODO"]

--[120, 456, 3213, 67, -100, -56, -20, 112, 354]
haloka2 ls = filter (\x -> mod x 10 /= 0) ls

tetel_1_3 = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
    putStrLn "A lista elemei: "
    let filteredls = filter zero_filter ls
    mapM_ (\x -> putStr $ show x ++ " ") filteredls
    

fgvlwthn ls n = filter (\x -> x < n) ls
fgvhithn ls n = filter (\x -> x > n) ls

tetel_2_1 = do
    let n = 6.5
    let ls = [9.5, 10, 6.5, 4.5, 5.5, 7, 7.25, 7.8, 9, 3.5, 6.5, 9.75, 3.5]
    let srt_ls = sort ls
    let sizels = length ls
    let lskisseb = fgvlwthn ls n
    let n_kisseb = length lskisseb
    let lsnagyobb = fgvhithn ls n
    let n_nagyobb = length lsnagyobb
    putStr "Nagyobb jegyek: "
    mapM_ (\x -> putStr $ show x ++ " ") lsnagyobb
    putStr ("Szamuk: " ++ show n_nagyobb ++ "\n" ++ show n ++ "-el egyenlo jegyek szama:" ++ show (sizels - n_kisseb - n_nagyobb) ++ "\nKissebb jegyek: ")
    mapM_ (\x -> putStr $ show x ++ " ") lskisseb
    putStrLn "\n"

vagoka [] _ = ("", "")
vagoka (x:xs) megallo 
    | x == megallo = ("",xs)
    | otherwise = let (talalat, maradek) = vagoka xs megallo in (x:talalat, maradek)

datumErtek s = let (honap, ev) = break (== '/') s
               in read (drop 1 ev ++ honap) :: Int
tetel_2_2 = do 
    print "=====A======="
    let ls = [("rosalesanthony@example.net", "03/31", "213130957725524"),("robin18@example.net", "02/29", "570620146482"),("bsullivan@example.org", "03/27", "4215057708441701869"),("jameshughes@example.org", "09/27", "4782851642138996"),("douglasjordan@example.net", "03/27", "5289954454350249"),("jwells@example.net", "06/31", "342926219737676"),("spotter@example.com", "01/27", "4917299108623093")]
    
    let ls_help = sort $ map (\(i, (a, b, c)) -> ((datumErtek b, a, c), i)) $ zip [0..] ls
    let indexes = take 4 $ map(\((datum, email, id),i) -> i)ls_help
    
    mapM_ (\i -> let (email,datum,id ) = (ls !! i) in putStr $ email ++ ", " ++ datum ++ ", " ++ id ++"\n") indexes 
    
    
    
    putStrLn "=====B=======" 
    let bontott_ls = map (\(email, datum, id) -> 
                        let (nev, maradek) = vagoka email '@' 
                            (dom, veg)     = vagoka maradek '.' 
                        in (nev, dom, veg)                   
                     ) ls
    putStrLn "Nev\t\t\tDomain\t\t\tVeg: "
    mapM_ (\(nev, dom, veg) -> putStrLn $ nev ++ "\t\t\t" ++ dom ++ "\t\t\t" ++ veg) bontott_ls