-----------------------------
---------- Diapo 41 ---------
-----------------------------
-- "abc" = ["a", "b", "c"] et  "" = []. String = [Char] (alias)

-- import Data.List -- pour delete
-- an::String->[String]
-- an "" = [""]
-- an xs =   concat $ map (\a -> map (a:) (an (delete a xs))) xs

-- main = print(an "abc")
-- output ["abc","acb","bac","bca","cab","cba"]

-----------------------------
---------- Diapo 42 ---------
-----------------------------
-- divise n k = mod n k == 0 -- True if k|n
-- diviseurs n = filter (divise n) [1..n]
-- diviseurs n = filter ((==0).(mod n)) [1..n] -- Le point est la composition mathématique

-- premiers n = filter (\x -> length (diviseurs x) == 2) [2..n]

-- main = print(premiers 15)


-----------------------------
---------- Diapo 46 ---------
-----------------------------
-- 1
-- sumL xs = foldl (+) 0 xs
-- ou 
-- sumL = foldl (+) 0
-- main = print(sumL [1,2,3])

-- 2
-- maxL::Ord a => [a] -> a
-- maxL xs = foldl1 max xs 
-- main = print(maxL [1,5,3])  -- output 5
 
-- 3
-- andL = foldr (&&) True
-- main = print(andL [True, False, True])
-- ou
-- andL = foldl (&&) True
-- main = print $ andL [True,True,False] 

-- 4
-- f x = x == 0 -- prédicat de test
-- anyL f xs = or $ (map f xs)
-- or xs = foldr (||) False xs
-- main = print(anyL f [1,2,0,3,4])

-- 5
-- concatL = foldl (++) []
-- main = print(concatL [[1,2], [3,4,5], [6]])



main = print $ concat [[1,2], [3,4,5], [6]]


-----------------------------
---------- Diapo __ ---------
-----------------------------
-- diviseurs n = filter ((==0).(mod n)) $ takeWhile (not.(>n).(^2)) primes
-- primes = 2:filter (null.diviseurs) [3..] 
-- null renvoie True si la liste est vide. On sort 2 pour avoir un cas de base
