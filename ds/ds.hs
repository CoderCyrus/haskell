-----------------------------
------1 Types algébriques
-----------------------------
-- Q 1.1 Sommes de multiples
data Entier a b  = Liste a b | Excep a b 
valeur:: Entier a b -> Integer  
valeur (Liste a b) = 3*a + 4*b
valeur (Excep a b) = a
main = print $ valeur  (Liste 1 1)

-- Q1 Hocine 
data Entier = Un | Deux | Cinq | Liste Integer Integer
valeur:: Entier -> Integer 
valeur entier = case entier  of 
      Un -> 1
      Deux -> 2
      Cinq -> 5
      Liste i j ->(3*i + 4*j)  

-- Q1.2 Coordonnées des points dans le plan
data Point = Axis Integer Integer | Angle Double Integer 
dOrigine:: Point -> Integer
dOrigine point = case point of 
       Axis a b -> sqrt (a*a + b*b)
       Angle x l -> l 

-- Q1.3
data ExprA = Val A  | Inc (ExprA  ExprA ) | Mul (ExprA ExprA )
evaluate::(Fractional a)  => ExprA   a -> a
evaluate (Val a) = a
evaluate (Inc e1 e2) = evaluate e1 + evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2
-----------------------------
------2 Récursivité
-----------------------------
-- 2.1 Algorithme d’Euclide https://fr.wikipedia.org/wiki/Algorithme_d%27Euclide_%C3%A9tendu
eucl(r, u, v, 0, u', v') = (r, u, v)
eucl(r, u, v, r', u', v') =   if r' /= 0 
                              then eucl(r', u', v', r - (r/r')*r', u - (r/r')*u', v - (r/r')*v')
                              else (r, u, v)
euclid(a, b) = eucl(a, 1, 0, b, 0, 1)

-- 2.2 
-- Q1 Écrire une fonction récursive qui calcule la somme des n premiers entiers
sommeN:: Int -> Int
sommeN 0  = 0
sommeN n = n + sommeN (n-1)
-- main = print $ sommeN 5 output: 15

-- Q2 Écrire une fonction récursive terminale qui calcule la somme des n premiers entiers
sommeN':: Int -> Int -> Int
sommeN' 0 acc = acc
sommeN' n acc = sommeN' (n-1) (n+acc)

sommeNT:: Int -> Int
sommeNT n = sommeN' n 0 
-- main = print $ sommeNT 5

-- Q3 Écrire une fonction récursive terminale qui teste si un nombre est parfait
sumN :: Int -> Int -> Int -> Int
sumN  _ 0 acc = acc
sumN n k acc = if n `mod` k == 0
               then sumN n (k-1) (acc+ k )
               else sumN n (k-1) acc

sumNFinale :: Int -> Int
sumNFinale n = sumN n (n-1) 0
-- main = print $ sumNFinale 6  -- output 18

sumTest :: Int -> Bool
sumTest n = ( (sumNFinale n)  == n )
-- main = print $ sumTest 6  -- output : True

----------------------------- ?
-- 2.3 Suite de Syracuse ? 
syracuse:: Int -> Int -> Int
syracuse x 0 = x
syracuse x n = let val = syracuse x (n-1) in if odd val
                then 3*val + 1
                else div val  2

----------------------------- ? presque 
-- 2.4 Expressions bien parenthésées
-- import Data.List
-- countN:: Int -> Int -> Int
-- countN  _ n k = 0 
-- countN (x:xs) n k  = if x == '('
--                     then count xs (n+1) k 
--                     else count xs n (k+1)

-- sumN l = countN  l 0 0 
-- main = print $ sumN "()())"

----------------------------- ? résultat dicord 
-- 2.5 Chemin de plus petite somme
-- tail le rest 
mincouple::(Int,Int)->Int
mincouple (a,b) = if a>b then b else a

minPath::[[Int]]->Int
minPath [] = 0
minPath t = head (head t) +
    if length (head t)==1 
    then minPath (tail t) 
    else  (if length t == 1 then minPath [tail x|x<-t] else mincouple (minPath [tail x|x<-t] , minPath (tail t)))
-- main = print $ tail [[1,2,3],[8,9], [888]]

--2.6  Puissances de fonctions
puissancef::(a -> a) -> a -> Int -> a
puissancef a b 0  =  b
puissancef a b c  = puissancef  a  (a b)   (c-1) 
-- main = print $  puissancef (+1) 2 3 
-- output 2 +1 + 1 + 1 = 5 

-- 2.7 Éléments d’indices pairs dans une liste
-- Q1 le nombre de chiifre pair dans une liste
listePair:: [Int] -> [Int]-> [Int]
listePair [] ls   = ls
listePair (x:xs) l  = if x `mod` 2 == 0 
                   then listePair xs (l++[x])
                   else listePair xs l

-- main = print $ length $ listePair [1..10] []  -- output 5
-- Q2 zip filter map
auPair:: [Int] -> Int
auPair ls = length $ filter (\x -> x `mod` 2 == 0) ls
main = print $  auPair [1..10]

-- 2.8 Fonctions drop, take et splitAt
-- Q1 drop
dropM:: Int -> [Int] -> [Int]
dropM 0 ls = ls
dropM n (x:xs) = if n > (length (x:xs))
               then []
               else dropM (n-1) xs
-- main = print $ dropM 6 [4,5,6,7,8,9] 
-- Q2 récursive terminale
takeM::Int -> [Int]-> [Int]-> [Int]
takeM 0 ls l  = l
takeM n (x:xs) l  = if n > length (x:xs)
                    then (x:xs)
                    else takeM (n-1)  xs  (l++[x]) 
-- main = print $ takeM 3 [4,5,6,7,8,9] []
----------------------------- ? 
-- Q3 Sans utiliser de récursion explicite ??
splitAtM:: Int -> [Int] -> ([Int],[Int])
splitAtM 0 l = ([],l)
splitAtM n liste = (dropM ((length liste) - n ) liste , takeM n liste [] )
-----------------------------
------3 Utilisation de map
-----------------------------
-- 3.1 liste de liste
map2D:: (a->b) -> [[a]] -> [[b]]
map2D f mat = map (map f) mat
main = print $ map2D (+1) [[1,2,3],[4,5,6],[7,8,9]] 
-- 3.2  
puissanceX :: Integer -> Integer -> Integer
puissanceX x i = x^i

petitePuissance:: Integer -> Integer -> Integer
petitePuissance x y = (filter (>y) (map (puissanceX x) [1..y] )) !!0

main = print $ petitePuissance 3 35  --output 81
-- 3.3 Chiffre de César
----------------- à faire 
import Data.Char 
cesar n mot = map ( \x -> chr $ (ord 'a' + ( mod (ord  x - ord 'a' + n )  26 ) ))  mot



-----------------------------
------4 Utilisation de filter
-----------------------------
----------------------------- ? 
-- 4.1 Sous-listes croissantes 
subMax:: [Int] -> [Int]
subMax [] = []
subMax (x:xs) = filter (>x) xs
main = print $ subMax [9,11,34,1,2,3]
----------------------------- ? 
-- 4.2 Somme des éléments pairs dans une liste
sommePairs::[a] -> Int
sommeParis  = sum.(filier even)
main = print $ sommeParis [1,2,3,4] 

-----------------------------
------5 Utilisation de fold
-----------------------------
-- 5.1 Inversion d’une liste
reverseM :: [Int] -> [Int]
reverseM xs = foldl (flip (++)) [] (map (\x->[x]) xs)
-- 5.2 Valeur d’un polynôme en un point
evalPoly ls n = foldl (\x y -> y*n^((length (reverse ls))-1)+x) 0 (reverse ls)
main = print $ evalPoly [1,2,3] 4.0 
-- 5.3 Map et fold
-- Q1 
mapWithFold::(a->b) -> [a] -> [b]
mapWithFold f list = reverse $ foldl (\resList x -> (f x):resList) [] list
main = print $ (mapWithFold (*3) [1..10]) == (map (*3) [1..10]) -- expected: True
-- Q2 pas possible
le type foldl (a->b->a)->(a->[b]->a) et pour type map (a->b)-> [a]->[b] 
donc à mon avis on ne peux pas obtenir une valeur avec une liste non récursive
-----------------------------
------6 Listes en compréhension
-----------------------------
-----------------
-----  6.1  -----
-----------------
argminf::(Int -> Int) -> Int -> Int
argminf f n = head $ [x | x<-[0..], f x > n]
-- main = print $ argminf (+2) 10 -- expected: 9


-----------------
-----  6.2  -----
-----------------
produitMat::[[Int]] -> [[Int]] -> [[Int]]
-- a et b matrices n x n
produitMat a b = let n = (length a) in
    [[sum $ [(a!!i!!k) * (b!!k!!j) | k<-[0..(n-1)]] | j<-[0..(n-1)]] | i<-[0..(n-1)]] 
-- main = print $ produitMat [[1,2],[3,4]] [[5,6],[7,8]] -- expected: [[19,22],[43,50]]

