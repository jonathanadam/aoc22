import System.IO
import ProcessInputs
import Data.List

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let stuff = lines contents
  let elves = map (map stringToInt) (splitOn "" stuff)
  let totalCalories = map sum elves
  let sortedCalories = sortBy (flip compare) totalCalories
  let topThree = take 3 sortedCalories
  print (maximum totalCalories)
  print (sum topThree)
  hClose handle
