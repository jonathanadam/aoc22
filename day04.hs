import System.IO
import ProcessInputs
import Data.List
import qualified Data.Set as Set
import Data.Char

findRange s = (head parsed, head $ tail parsed)
  where parsed = map stringToInt $ splitOn '-' s

isCompleteOverlap [(a, b), (c, d)] = 
   (x `Set.isSubsetOf` y) || (y `Set.isSubsetOf`x)
  where x = (Set.fromList [a..b]) 
        y= (Set.fromList ([c..d]))

hasOverlaps [(a, b), (c, d)] = not $ null $ x `Set.intersection` y
  where x = (Set.fromList [a..b]) 
        y= (Set.fromList ([c..d]))

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let pairs = map (splitOn ',') $ lines contents
  let parsedPairs = map (map findRange) pairs
  let completeOverlaps = length $ filter (id) $ map isCompleteOverlap parsedPairs
  print completeOverlaps
  let overlaps = length $ filter id $ map hasOverlaps parsedPairs
  print overlaps
  hClose handle
