-----------------------------------------------
---1 Structures de données de base 
-----------------------------------------------
-- type somme 
data Color = Red | Yel deriving Eq
data Cell = Empty | Full Color deriving Eq 
type Column = [Cell]
data Grid = Grid [Column]

-----------------------------------------------
---2 Créer la grille vide et afficher
-----------------------------------------------
-- Q4
initial::Grid
initial = Grid $ replicate  7 ( replicate 6 Empty)

-- Q5 une instance de classe type Show
instance Show Cell where
    show Empty = ". "
    show (Full Red) =  -- TODO
    show (Full Yel) =  -- TODO

-- Q6
-- transpose matrix en ligne à matrix en column
instance Show Grid where
    show (Gird l ) = unlines $ map (concatMap show ) (transpose l) 

instance Show Color where
    show Red = "rouge"
    show Yel = "jaune"

-----------------------------------------------
---3 Jouer un coup
-----------------------------------------------
-- Q7
ddToken::Column->Color->Column
ddToken column color = 
    let (top,buttom) = span((==) Empty) column in 
    let legth top == 0 then column 
    else (tail top) ++ (Full color):buttom




