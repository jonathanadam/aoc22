import System.IO
import ProcessInputs
import Data.List
import Data.Char

splitHalf l = splitAt ((length l) `div` 2) l
unique (a,b) = head $ intersect a b
findbadge [a, b, c] = head $ intersect c $ intersect a b

scoreItem a | ord a > 96 = ord a - 96
scoreItem a  = ord a - 38

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let bags = lines contents
  let uniques = map (unique . splitHalf) bags
  let itemScores = map scoreItem uniques
  print $ sum itemScores
  let groups = groupByNs 3 bags
  let badgeScores = map (scoreItem . findbadge) groups 
  print $ sum badgeScores
  hClose handle
