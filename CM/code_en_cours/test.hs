data PossibleReel = Rien | Valeur Double deriving Show -- constructeur de donnée

inversion :: Double -> PossibleReel
inversion 0 = Rien 
inversion x = Valeur (1 / x)
--Rien et Valeur sont *implicitement* définies avec ce type
--Rien :: PossibleReel
--Valeur :: Double -> PossibleReel

opposePR :: PossibleReel -> PossibleReel
opposePR Rien       = Rien 
opposePR (Valeur x) = Valeur (-x)

--main = print ( inversion 2.9)
--main = print (opposePR (Valeur 2.2))
      
-- Le type PossibleReel exite, sous la forme d'un type paramétré
-- pour tout type a...
data Maybe a = Nothing | Just a -- Maybe est un constructeur de type
-- pour tous types a et b...
data Either a b = Left a | Right b
safeTwice:: Int-> Either Int Int
sateTwice n = if n > (maxBound :: Int) 'div' 2
                then Left n
                else Right(2*n)
