import Data.Set (Set) -- on n’importe que Set
import qualified Data.Set as S -- puis tout mais en qualifiant
import System.IO
import System.Random
import Data.List
-----------------------------------------------
---1 Structures de données de base et affichage
-----------------------------------------------
-- Q1 
-- Covered(couverte) : si elle contient une mine, ou si elle est marquée par un drapeau
-- Uncovered : le nombre de mines 
data Cell = Covered Int Bool Bool| Uncovered Int | Selected 

-- Q2
data Grid = Grid [[Cell]]

-- Q3
instance Show Cell where 
    show Selected = "x"
    show (Uncovered _ ) = " "   
    show (Uncovered n) = show n  -- n mines
    show (Covered _ _ True ) = "!" -- un drapeau
    show (Covered _ _ False) = "#" 

-- Q4 une instance de la classe de types Show pour Grid, afin d’afficher la grille
-- unlines : creates a string from an array of strings, it inserts new line characters between original strings
--main = print $ unlines ["Hello","World","!"]   -- output  "Hello\nWorld\n!\n"
-- concatMap
instance Show Grid where 
    show (Grid list) = unlines $ map (concatMap show ) list 

-----------------------------------------------
---2  Créer la grille avec les mines
-----------------------------------------------  
-- Q5 
randSet:: Int -> StdGen -> StdGen -> Int -> Int -> Set(Int,Int)
randSet n genRow genCol nRow nCol = 
    let randomRow = randomRs(0,nRow-1) genRow in 
    let randomCol = randomRs(0,nCol-1) genCol in 
    let listSets = scanl' (flip S.insert) (S.empty) (zip randomRow randomCol) in 
    head (dropWhile (\x -> S.size x < n ) listSets)

-- Q6
-- return a Covered cell without flag abd iff cellCoord is present in minesCoord
initCell:: Set(Int,Int) ->  (Int, Int) -> Cell  
initCell minesCoord cellCoord = 
    if S.member cellCoord minesCoord then
        Covered 0 True False
        else 
        Covered 0 False False

grid:: Int -> Int -> Set (Int, Int) -> Grid
grid nRow nCol minesCoord = Grid $ map (map (initCell minesCoord)) [[(i,j) | i <-[0..nRow]]| j <-[0..nCol]]

-----------------------------------------------
---3 Calculer le nombre de mines dans le voisinage
-----------------------------------------------  
-- Q7
-- pattern matching
mineIndic:: Cell -> Int
mineIndic( Covered _ True  _ )  = 1
mineIndic( Covered _ False _ )  = 0

-- Q8
mines::Grid -> [[Int]]
mines (Grid g)=  map  (map mineIndic ) g

-- Q9
moveUp::[[Int]] -> [[Int]]
-- tail : it accepts a list and returns the list without its first item
moveUp l = (tail l) ++ [replicate (length(head l)) 0]

-- Q10
moveDown::[[Int]] -> [[Int]]
moveDown [] = []
moveDown l = [replicate (length (head l)) 0] ++ (init l)
-----------------------------------------------------------
-- Q11 
moveLeft::[[Int]] -> [[Int]]
moveLeft l = transpose (moveUp (transpose l))

moveRight::[[Int]] -> [[Int]]
moveRight l = transpose $ moveDown $ transpose l

-- Q12
-- version Muruo
-- gridMoves::[[Int]] -> [[Int]]
-- gridMoves mines = 
--             let up   = moveUp mines in 
--             let down = moveDown mines in 
--             tail $ concat $ map (\x -> [x, moveLeft x, moveRight x]) [mines, up, down]  -- tail pour supprimer la liste originale de mines
-- version Ben
gridMoves::[[Int]] -> [[[Int]]]
gridMoves mines = 
    let up = moveUp mines in
    let down = moveDown mines in
    tail $ concat $ map (\x -> [x, moveLeft x, moveRight x]) [mines, up, down] -- tail pour supprimer la liste initiale (mines) 

-- Q13 
-- liste dans liste -> deux map deux zipWith
-- zipWith
matrixSum::[[Int]] -> [[Int]] -> [[Int]]
matrixSum matA matB = zipWith (zipWith (+)) matA matB 
--main = print $ matrixSum [[1,2,3], [4,5,6]] [[1,2,3], [4,5,6]]

-- Q14
neighbourMap::Grid -> [[Int]]
neighbourMap g =  foldl1'  matrixSum (gridMoves $ mines g )   -- pas besion de base du cas

-- Q15 
updateCell::Cell->Int->Cell
updateCell (Covered _ mine flag )  n = Covered n mine flag
updateCell (Uncovered _ )  n = Uncovered n 
updateCell Selected _ = Selected 

-- Q16
updateGrid::Grid -> [[Int]] -> Grid
updateGrid (Grid g) neigh  = Grid $ zipWith (zipWith updateCell ) g neigh

-----------------------------------------------
---4  Découvrir une case
-----------------------------------------------  
-- Q17
-- une liste xs de a
applyi:: (a->a) -> Int -> [a] -> [a]
-- f fonction 
-- int : indice
-- [a] liste entrée
applyi f i xs  =  let
    (begin,end) = splitAt i xs
    in begin ++ (f $ head end ) : tail end
-- main = print $ applyi (\x->3*x) 3 [1,1,1,1,1]  -- output [1,1,1,3,1] (chiffre commerce par 0)
-- main = print $ splitAt 3 [1,1,1,1,1]  -- output ([1,1,1],[1,1])

-- Q18
-- une liste xss de listes de a
applyij::(a->a) -> Int -> Int -> [[a]] -> [[a]]
-- pattern matching
applyij f i j xss = applyi (applyi f j ) i xss
-- main = print $ applyij (\x -> 3*x) 2 3 [[1,1,1,1,1], [1,1,1,1,1], [1,1,1,1,1]] -- chiffre commerce par 0

-- Q19
-- découvrir récursivement toutes les cases autour
-- const une fonction constante
-- pass le status Covered à Uncovered
uncover::Int -> Int -> Grid -> Grid
uncover i j (Grid l) = case l!!i!!j of 
    Uncovered _ -> (Grid l)

    -- 0 mine découvrir les cases autour
    Covered 0 _ _ -> let g' = Grid $ applyij (const $ Uncovered 0) i j l in  -- on découvre la case actuelle et mettre en 0 (0 mine)
                     let voisins = [(i+i', j+j') | i'<-[-1,0,1], j'<-[-1,0,1], i+i'>=0, j+j'>=0, i+i'<(length l), j+j'<(length $ head l), (i+i', j+j') /= (0,0)] in  -- TODO
                     foldl' (\grid (i', j') -> uncover i' j' grid) g' voisins  -- on découvre récursivement tous les voisins

    -- n mines découvrir les cases autour
    Covered n _ _ -> Grid $ applyij (\(Covered n _ _)-> Uncovered n) i j l

-----------------------------------------------
---5 Boucle principale de jeu (Read-Eval-Print Loop, REPL)
-----------------------------------------------  
-- Q20
covIndic::Cell -> Int
covIndic Covered _ _ _ = 1
covIndic Uncovered _ = 0

-- Q21 won
-- sum
-- on a gagné quand le nombre de cellules couvertes = nombre de mines
won::Int -> Grid -> Bool
won n (Grid l) = (sum $ map (sum.(map covIndic)) l) == n

-- Q22
toggleFlag::Cell -> Cell
toggleFlag Covered n mine flag = Covered n min (not flag)
toggleFlag c = c -- ne fait rien sur les autres cellules

-----------------------------
--- maybeFindMine TODO
-----------------------------



-- Q23 version B
-- Compléter la fonction loop
loop :: Int -> Int -> Int -> Grid -> IO ()
loop i j n b@(Grid xs) -- le paramètre b se décompose en (Grid xs)
    | won n b = putStrLn "Victoire !" 
    | otherwise = do  
        -- affiche la grille avec la case i, j sélectionnée
        putStrLn $ show $ Grid $ applyij (const Selected) i j xs 
        -- lit un caractère
        c <- getChar
        case c of
            'i'       -> loop (max (i - 1) 0) j n b -- bouge le curseur vers le haut
            'k'       -> loop (min (i + 1) ((length xs) - 1)) j n b -- bouge le curseur vers le bas
            'j'       -> loop i (max (j - 1) 0) n b -- bouge le curseur vers la gauche
            'l'       -> loop i (min (j + 1) ((length (xs!!0)) - 1)) n b -- bouge le curseur vers la droite
            'f'       -> loop i j n (Grid $ applyij toggleFlag i j xs) -- pose ou enlève un drapeau sur la case i, j
            'u'       -> case (xs!!i!!j) of 
                            Covered _ True _ -> putStrLn "BOOM ! Défaite..."
                            _ -> loop i j n (uncover i j b) -- découvre la case i, j; BOUM ?
            't'       -> case (maybeFindMine b) of
                            Nothing -> loop i j n b
                            Just (i', j') -> loop i' j' n b
            otherwise -> loop i j n b -- ne fait rien    
            
-- Q24
main :: IO () 
main = do
    -- désactive l’attente de la touche entrée pour l’acquisition
    hSetBuffering stdin NoBuffering
    -- désactive l’écho du caractère entré sur le terminal
    hSetEcho stdin False
    -- récupère deux StdGen pour la génération aléatoire
    g <- newStdGen
    g' <- newStdGen
    -- nombre de mines, lignes, colonnes
    let nmines = 5
    let l = 7
    let c = 10
    -- créer la grille, ajouter les mines, mettre à jour les voisins
    let minesCoord = randSet nmines g g' l c
    let b' = grid l c minesCoord
    let b = updateGrid b' (neighbourMap b')
    loop 0 0 nmines b -- démarrer la REPL
         


    

               










