import Data.List 
import Data.Char (isDigit)
feladat_1 n ls = map fst (filter(\(x,y) -> y > n) ls)


feladat_11 = do
    let n = 150000
        ls = [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000), ("marosvasarhely", 130000), ("arad", 160000), ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
        ls2 = sort $ feladat_1 n ls

    if ls2 == []
        then putStrLn ("Nincs" ++ show n ++ "erteknel nagyobb nepesseg ertekkel rendelkezo varos.")
        else do
            putStrLn ("A(z) " ++ show n ++ "nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok: ")
            mapM_ (\v -> putStrLn ("- " ++ v)) ls2


nincsSzam x = not (any isDigit x)
fel3 = do 
    let ls = ["2023tuple", "function", "float", "higher-order", "variable10","may13be", "0recursion", "monad", "class"]
        jo = sort $ filter nincsSzam ls
    if jo ==[]
        then putStrLn "Nincsenek olyan karaktelancok melyek nem tartalmaznak szamot."
        else do
            putStrLn "A karakterlancok amelyek nem tartalmaznak szamokat: "
            mapM_ putStrLn jo