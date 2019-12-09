import Data.Char;
import Data.List;

-- constructor de donnée
-- deriving Show : affichier  data Message en String
data Message = Mes [Integer]  deriving Show 

-- Q2
stringToMessage str = Mes $ map (fromIntegral.ord) str
messageToString (Mes msg) = map (chr.fromIntegral) msg
-- main = print $ messageToString  $ Mes [72,101,108,108,111]  -- output "Hello"

-- Q3 padding
pad size (Mes msg) = let r = size - (mod (length msg) size) in 
            if r == 0 then 
                Mes(msg ++ replicate (fromIntegral size) (fromIntegral size))
            else 
                Mes(msg ++ replicate (fromIntegral r) (fromIntegral r))
--main = print $ pad 5 (stringToMessage "Hello" )  --output Mes [72,101,108,108,111,5,5,5,5,5]

-- Q4 unpadding 
-- supprimer les chiffres ajoutés 
unpadInt :: [Integer] -> Integer -> [Integer]
unpadInt msg  0 = msg
unpadInt (x:xs) padSize = unpadInt xs (padSize-1)
-- reverse la table
unpad :: Message -> Message
unpad (Mes []) = (Mes [])
unpad (Mes msg) = let (padSize:xReverse) =  reverse msg in Mes(reverse (unpadInt (padSize:xReverse) (fromIntegral padSize)))
--main =  print $ messageToString $ unpad $ Mes [72,101,108,108,111,5,5,5,5,5]  -- output "Hello"

-- Q5  groupBytes
groupBytes :: [Integer] -> Integer
groupBytes [] = 0
groupBytes (x:xs) = x*256^(length xs) + groupBytes xs
--main =print (groupBytes [128,54,33,99])   -- output 2151031139

-- Q6
ungroupBytes ::Int->Integer->[Integer]
ungroupBytes 0 _  = [0]  -- cas quand bsize est 0
ungroupBytes 1 n  = [n]
--ungroupBytes bsize n base =  take (fromIntegral bsize) (iterate (base*) (mod n base))
ungroupBytes bsize n  = let a = (256^(bsize -1)) in let q = ( n `div` a) in q:(ungroupBytes (bsize-1) ( n `mod` a))
--main = print $ ungroupBytes  4 2151031139 

-- Q7 
-- Input: splitAt 5 [1,2,3,4,5,6,7,8,9,10]
-- Output: ([1,2,3,4,5],[6,7,8,9,10])
groupN :: Int -> [Integer] -> [[Integer]]
groupN bsize [] = []
groupN bsize msg  = let res= splitAt bsize msg in [fst res] ++ groupN (bsize) (snd res) 
-- main = print $ groupN 5 [1,2,3,4,5,6,7,8,9,10]  -- output [[1,2,3,4,5],[6,7,8,9,10]]

-- Q8
-- groupN groupBytes
makeBlocks::Int-> Message -> Message
makeBlocks bsize (Mes msg) = Mes(map groupBytes (groupN bsize msg))
--main = print $  makeBlocks 2  (Mes [1,2,3,4])  -- output Mes [258,772]

--Q9 
-- [[a]] -> [a]
-- Input: concat [[1,2,3], [1,2,3]] 
-- Output: [1,2,3,1,2,3]
splitBlocks::Int -> Message -> Message
splitBlocks bsize (Mes msg) = Mes(concat(map (ungroupBytes bsize) msg ))
--main = print $ splitBlocks 2 (Mes[258,772])

-- Chiffrement et déchiffrement