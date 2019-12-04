-- évaluation conditionnel
-- f :: Integer -> Integer
-- f n =  if n == 0 then 1
--        else n * f (n-1)
-- main :: IO ()
-- main = print (f 5)

-- filtrage par motif (Pattern matching)
-- f 0 = 1
-- f n = n * f (n-1)

-- f n = case n of 
--     0 -> 1
--     _ -> n * f (n-1)

-- main = print(f 5)

-- Les noms de fonctions et variables 
-- commencent par une minuscule
-- square x = x*x
-- inc x = x + 1

-- f x = (inc.square) x  -- ou juste f x = inc (square x)
-- main = print( f 3 )

-- évaluation gradée
-- signe n
--     | n < 0     = -1
--     | n > 0     = 1
--     | otherwise = 0
-- main = print(signe 10)

-- fonction récursive terminale
-- f' 0 r = r
-- f' n r = f' (n-1) (n*r)
-- f n = f' n 1
-- main = print (f 3)

-----------------------------
------- slide 15 ------------
-----------------------------
--15.1.1
-- puiss x 0 = 1
-- puiss  x n = x * ( puiss  x (n-1)) 
-- main = print (puiss  4 4 )

--15.1.2
-- p x n = case n of 
--    0 -> 1
--    _ -> if (even n)
--        then p ( x*x ) (div n 2)
--        else x* p (x*x) (div n 2)
-- main = print (p 4 3)

--15.2.1
-- fibo 0 = 1
-- fibo 1 = 1
-- fibo n = fibo(n-1) + fibo(n-2) 
-- main = print (fibo 4)

-----------------------------
------- slide 16 ------------
-----------------------------
-- r ::Integer->Integer    -- fonction donnée
-- raux n x = if n == 0
--     then x
--     else raux (div n 10) (10*x + (mod n 10))
-- r n = raux n 0
-- main = print (r 123)

-- hpal n = if (r n) == n  -- sortie est un Integer
--     then 0 
--     else 1 + hpal(n + r n)
-- main = print (hpal 454)

-- déclaration locale 
-- sommeCube x y = let  z = x + y in z*z*z
-- main = print(sommeCube 2 1)
-- sommeCube x y = let  add u v = u +v  
--                      z = x +  y 
--                 in z*z*z
-- main = print(sommeCube 2 1)

-----------------------------
------- slide 22 ------------
-----------------------------
--écrire une fonction et qui réalise le et logique entre deux Bool
-- et :: Bool -> Bool -> Bool
-- et True True = True
-- et _ _ = False
-- main = print(et True True)

-----------------------------
------- slide 23  :)---------
-----------------------------
-- data Point = Coord Double Double
-- distance :: Point -> Point -> Double
-- distance (Coord x1 y1 ) (Coord x2 y2 ) = sqrt( (x1-x2)^2 + (y1-y2)^2 )
-- -- main = let p1 = Coord 1.1 1.1 in print (distance p1 (Coord 3.1 3.1))

-- data Figure =  FigP Point | FigC  Double| FigCa Point Point --constructeur de donnée
-- -- type de sortie de donnée est Double
-- perimetre(FigP _ ) = 0.0  
-- permetre(FigC r) = 2*3.14*r
-- permetre(FigCa p1 p2) = (distance p1 p2 )* 2 * sqrt 2
-- main = print (perimetre(FigP (Coord 2.1 2.1)))
-- main = print (permetre (FigCa (Coord 2.1 2.1)  (Coord 3.1 3.1)))
-- main = print (permetre(FigC 2.1))

-- Les tuples
-- fsd et snd pour les couples
-- maxi ::(Integer, Integer)-> Integer
-- maxi x = let a = fst x   
--              b = snd x in 
--              if a > b then a else b
-- main =print (maxi (20,3))

-- pattern matching 
-- maxi ::(Integer, Integer)-> Integer
-- maxi (a,b) = if a > b then a else b
-- main =print (maxi (20,3))

-- pas que pour les couples
-- ror :: (Char, Char,Char) ->(Char, Char,Char)
-- r(a,b,c) = (c,b,a)
-- main = print (r)

-- data Canard = Coin {nom::String, enverg:: Double}

-- info::Canard-> String
-- info c= "Oh le beau canard " ++ (nom c) ++
--     " d'envergure " ++ show (enverg c) ++ "   "

-- main = let c1 = Coin "Coin" 0.5
--            c2 = Coin {nom = "Gaga" , enverg = 0.2}
--            c3 = c2{enverg= 0.3}
--         in
--             print(info c1 ++ info c3) 

-----------------------------
------- slide 28 comment tester------------
-----------------------------
-- data Nat = Zero | Succ Nat
-- intVal :: Nat -> Integer
-- intVal Zero = 0
-- intVal (Succ x) = (intVal x) + 1

-- addition :: Nat -> Nat -> Nat
-- -- addition Zero n = n
-- -- addition Zero (Succ x) = Succ x
-- additoin (Succ x) y = addition x (Succ y)  -- ou Succ(additon x y )
-- addition (Succ x) Zero = Succ x
-- addition (Succ x) (Succ y) = addition x (Succ (Succ y))
-- main = print (intVal(addition (Succ Zero) (Succ Zero) ))

-----------------------------
------- slide 29 ------------
-----------------------------
-- data Liste = Vide | Cons Integer Liste  --comme pointeur dans C
-- somme :: Liste -> Integer
-- somme Vide = 0
-- somme (Head x l) = x + somme l --permettre de parcourir tous les élément dans la liste

-- data ArbreBinaire = VideA |  Head Integer ArbreBinaire ArbreBinaire
-- hauteur VideA = 0
-- hauteur (Noeud x g d)= 1 + max (hauteur g) (hauteur d) --gauche et droit

-- data Either a b = Left a | Right b deriving Show 
-- safeTwice:: Int-> Either Int Int
-- sateTwice n = if n > 300
--                 then Left n
--                 else Right (2*n)

-- main = print (safeTwice(200))

-- Ex 31
-- triple = [(a,b,c) | a<-[0..333], b<-[(a+1)..500], c<-[(b+1)..1000], a+b+c == 1000 , a^2+b^2 == c^2]  
-- main = print triple

-- -- pour tout tyoe a 
-- head :: [a] -> a
-- head [] = error ("head: Empty list")
-- head (x:xs) = x
-- -- dans Data.Maybe
-- listToMaybe::[a]->Maybe a
-- lsitToMaybe [] = Nothing
-- lsitToMaybe (x:xs) = Just x

------------------------------------------------------------------- 02/12
-- diapo 55
import Data.List

-- main::IO ()
--1
-- main = getLine >>= (\x->putStrLn(x ++ " " ++ reverse x))
--2
--muruo 
--main = getLine >>= (\x -> ( getLine >>= ( \y->putStrLn(y++ " " +x))) )

--main = getLine >>= (\x -> getLine >>= (\y -> putStrLn(y++ " "++ x)))

-- diapo 57
-- ioLength :: IO Int
-- ioLength = do
--     x <- getLine
--     return $ length x
-- main = do
--     x<- ioLength
--     print x
 
-- ioLength = getLine >>= return.length
-- main = ioLength >>= print

-- diapo 58
-- 1
-- import Data.Char
-- ord :: Char -> Int
-- chr :: Int -> Char
-- cesar n mot = map ( \x -> chr $ (ord 'a' + ( mod (ord  x - ord 'a' + n )  26 ) ))  mot
-- main = print (cesar 13 "coin")
-- prof
-- cesar :: Int -> String -> String 
-- cesar x mot =let a = ord 'a' in map (chr.(+a).(flip mod 26).(+x).(subtract a).ord) mot
-- ou ( \y -> chr(mod(ord y -a + x) 26) + a)
-- 2
-- main = do 
--     x <- getLine
--     let n = (read x) :: Int  -- pour être sûr que le type de conversation est bien
--     mot <- getLine
--     putStrLn $ cesar n mot
-- 3
-- getIntBorne :: IO Int
-- getIntBorne = do
--     putStrLn "Entrez une valeur "
--     x <- getLine
--     let n =(read x) ::Int
--     if n > 0 && n <= 25 then return n
--                         else getIntBorne
-- main = do 
--     x <- getLine
--     mot <- getLine
--     putStrLn $ cesar n mot

-- diapo 64  Foncteur
-- 1
data Expr a = Val a | Inc (Expr a) |Dec (Expr a)|Inv (Expr a)| Neg (Expr a)
-- 2
-- evaluate :: (Fractional a) => Expr a -> a
-- evaluate(Val x) = x  -- a est type ici
-- evaluate(Inc x) = (evaluate x) + 1
-- evaluate(Dec y) = (evaluate y) - 1
-- evaluate(Inv y) = 1/(evaluate y) 
-- evaluate(Neg y) = negate(evaluate y) --  or -(evaluate a) 
-- main = print ( evaluate (Val 2))
-- 3
main = print $ evaluate $ Dec $ Neg $ Inc $ Val $ 4  -- output = -6
-- 4
-- prof exemple
-- Maybe Double  soit Nothing soit Just 2.22
import Data.Maybe
isZero :: (Mum a ,Eq a) => Maybe a -> Bool
isZero Nothing  = False
isZero (Just x) = (x == 0)
-- 5
evaluate :: (Fractional a, Eq a) => Expr a -> Maybe a
evaluate(Val x) = Just x  
evaluate(Inc x) = let z = evaluate y in
                    if isNothing then Nothing 
                    else Just (From Just z+1)
evaluate(Dec y) = let z = evaluate y in
                    if isNothing then Nothing 
                    else Just (From Just z-1)
evaluate(Inv y) =   let z = evaluate y in 
                    if isZero z || isNothing z
                        then Nothing
                        else Just (1/From Just z)
evaluate(Neg y) = let z = evaluate y in
                    if isNothing then Nothing 
                    else Just (negate  $From Just z+1)
-- 6
-- fonction mevaluate
mfmap :: (a->b) -> Maybe a -> Maybe b
mfmap _ Nothing = Nothing
mfmap f (Just w) = Just $ f w
evaluate :: (Fractional a, Eq a) => Expr a -> Maybe a
evaluate(Val x) = Just x  
evaluate(Inc x) = mfmap (+1) $ evaluate x
evaluate(Dec y) = mfmap (subtract 1) $ evaluate y
evaluate(Inv y) = let z = evaluate y in 
                    if isZero z
                    then Nothing 
                    else mymap (1/) z
evaluate(Neg y) = mfmap negate $ evaluate y













