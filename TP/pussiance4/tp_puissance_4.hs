import Data.List
import Data.Maybe
import Data.Ord
import Data.Char
import System.IO


--------------------------------------
-- 1/ Structures de données de base --
--------------------------------------
data Color = Red | Yel deriving Eq
data Cell = Empty | Full Color deriving Eq
type Column = [Cell]
data Grid = Grid [Column]



-----------------------------------------
-- 2/ Créer la grille vide et afficher --
-----------------------------------------
initial::Grid
initial = Grid $ replicate 7 (replicate 6 Empty)

instance Show Cell where 
    show Empty = ". "
    show (Full Red) = "\x1b[31m" ++ "o " ++ "\x1b[0m"
    show (Full Yel) = "\x1b[33m" ++ "o " ++ "\x1b[0m"

instance Show Grid where
    show (Grid l) = unlines ((map (concatMap show) (transpose l)) ++ ["1 2 3 4 5 6 7 "])

instance Show Color where
    show Red = "Rouge"
    show Yel = "Jaune"

----------------------
-- 3/ Jouer un coup --
----------------------
addToken::Column -> Color -> Column
addToken column color = 
    let (top, bottom) = span ((==) Empty) column in
    if length top == 0 then column -- colonne pleine
    else (tail top) ++ (Full color):bottom

play::Grid -> Color -> Int -> Grid
play (Grid l) color k = 
    let (begin, column:end) = splitAt (k-1) l in 
    Grid $ begin ++ (addToken column color):end

-- main = print $ play (play initial Yel 3) Red 3



---------------------------
-- 4/ Trouver le gagnant --
---------------------------
addCellToList::[(Cell, Int)] -> Cell -> [(Cell, Int)]
addCellToList [] cell = [(cell, 1)] 
addCellToList l@((current, k):xs) cell = 
    if current == cell then (cell, k+1):xs
    else (cell, 1):l

summarize::[Cell] -> [(Cell, Int)]
summarize = foldl' addCellToList []

summarizeGrid::Grid -> [(Cell, Int)]
summarizeGrid (Grid l) = concat $ map summarize l

diagonalize::Grid -> Grid
diagonalize (Grid l) = 
    let nrow = length l in 
    let ncol = length $ head l in
    Grid $ [[l!!i!!(ncol-1-i-k) | i<-[0..(nrow -1)], (ncol-1-i-k) >= 0, (ncol-1-i-k) < ncol] | k <- [-(nrow-1)..(ncol-1)]]
-- main = print $ diagonalize $ [[1,2,3,4,5], [6,7,8,9,10], [11,12,13,14,15]]

winningPattern::(Cell, Int) -> Bool
winningPattern (Empty, _) = False
winningPattern (Full _, n) = n >= 4

allPatterns::Grid -> [(Cell, Int)]
allPatterns g@(Grid l) = summarizeGrid g ++ 
                (summarizeGrid $ Grid $ transpose l) ++
                (summarizeGrid $ diagonalize $ g) ++
                (summarizeGrid $ diagonalize $ Grid $ map reverse l)

won::Grid -> Maybe Color
won g@(Grid l) =
    let patterns = allPatterns g in
    let w = filter winningPattern patterns in 
    listToMaybe $ map (\(Full c, _) -> c) w



----------------------------------
-- 5/ Intelligence artificielle --
----------------------------------
legalMoves::Grid -> [Int]
legalMoves (Grid l) = filter (\i -> (head $ l!!(i-1)) == Empty) [1..(length l)]

evalPattern::(Cell, Int) -> Int
evalPattern (Empty, _) = 0
evalPattern (Full Red, k) = k * 100^k
evalPattern (Full Yel, k) = -k * 100^k

evaluate::Grid -> Int
evaluate g = sum $ map evalPattern (allPatterns g)


negaMax::Color -> Int -> Int -> Grid -> (Int, Int)
negaMax color depthMax depth g = 
    let moves = legalMoves g in 
    if (depth == depthMax) || ((length moves) == 0) then
        (negate $ evaluate g, 0)
    else
        let advColor = case color of
                        Red -> Yel
                        Yel -> Red in 
        let scoreInColumns = map (\i -> (negate $ fst $ negaMax advColor depthMax (depth + 1) (play g color i), i)) moves in
        maximumBy (comparing fst) scoreInColumns
   
        

--------------------------------------------------------------
-- 6/ Boucle principale de jeu (Read-Eval-Print Loop, REPL) --
--------------------------------------------------------------
-- Un joueur générique
-- Notez le IO Int car, pour un joueur humain, il faut lire au clavier 
class Contestant a where
    move::a->Grid->IO Int -- donner un coup à jouer 
    color::a->Color -- donner sa couleur

data Human = Hum Color 
data Computer = Com Color

instance Contestant Computer where
    move (Com c) g = return $ snd $ negaMax c 4 0 g
    color (Com c) = c

getMove::Grid -> IO Int
getMove g = do 
    putStrLn "Dans quelle colonne jouer ?"
    x <- getChar
    let k = digitToInt x
    let legal = legalMoves g
    if k `elem` legal then return k else getMove g

instance Contestant Human where
    move (Hum c) g = getMove g
    color (Hum c) = c

loop::(Contestant a, Contestant b) => Grid -> a -> b -> IO ()
loop g a b = do
    column <- move a g
    let gUpdated = play g (color a) column
    putStrLn $ show $ gUpdated
    let winner = won gUpdated
    case winner of
        Nothing -> loop gUpdated b a
        (Just color) -> putStrLn ("Victoire de " ++ (show color))


main :: IO () -- Point d’entrée dans le programme 
main = do
    -- désactive l’attente de la touche entrée pour l’acquisition
    hSetBuffering stdin NoBuffering
    -- désactive l’écho du caractère entré sur le terminal
    hSetEcho stdin False
    -- affiche la grille initiale
    putStr $ show initial
    -- lance la REPL
    loop initial (Hum Red) (Com Yel)

