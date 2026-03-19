myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myLength2 ls = foldr (\x db -> (+) 1 db) 0 ls

myLength3 ls = length ls

myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myProduct2 [] res = res 
myProduct2 (x : xs) res = myProduct2 xs (res*x)

myProduct3 ls = foldr (*) 1 ls

myMinimum [] = error "ures lista"
myMinimum [x] = x
myMinimum (x1 :x2 :xs)
    | x1 < x2 = myMinimum (x1 :xs)
    | otherwise = myMinimum (x2 :xs)

myMinimum2 ls = foldl1 min ls 

myMinimum3 ls = minimum ls

listaN ls n 
    | ls == [] = error "ures listi"
    | n < 0 = error "neg, index"
    | length ls <= n = error "tul nagy index"
    | otherwise = ls !! n

palindrom ls = ls == reverse ls

palindrom2 ls = if ls == reverse ls then "palindrom" else "nem palindrom"

palindrom3 [] = True
palindrom3 [x] = True
palindrom3 ls = head ls == last ls && palindrom3 ((init . tail) ls) -- levagja az elso s utolso elemet a listabol



szjls x
    | x < 0 = szjls (abs x)
    | x < 10 = [x]
    | otherwise = szjls (div x 10) ++ [mod x 10]

elsoUtolso ls = tail ls ++ [head ls]

elsoUtolso2 (x:xs) = xs ++ [x]


decP x p 
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

pDec :: (Foldable t, Integral b) => t b -> b -> b
pDec ls p = foldl(\hatvany x ->  x + (p ^ hatvany)) 0 ls

