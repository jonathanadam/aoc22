import System.IO
import ProcessInputs
import Data.List

data Move = Rock | Paper | Scissors 
  deriving (Eq, Show)

playFirstMove ("A":xs) = (Rock, xs)
playFirstMove ("B":xs) = (Paper, xs)
playFirstMove ("C":xs) = (Scissors, xs)

playSecondMove (Rock, ["X"]) = (Rock, Scissors)
playSecondMove (Rock, ["Z"]) = (Rock, Paper)
playSecondMove (Paper, ["X"]) = (Paper, Rock)
playSecondMove (Paper, ["Z"]) = (Paper, Scissors)
playSecondMove (Scissors, ["X"]) = (Scissors, Paper)
playSecondMove (Scissors, ["Z"]) = (Scissors, Rock)
playSecondMove (x, ["Y"]) = (x, x)

encodeGame l = playSecondMove ( playFirstMove l)

scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scorePoints (move1, move2)  | move1 == move2 = 3
scorePoints (Rock, Paper) = 6
scorePoints (Paper, Scissors)= 6
scorePoints (Scissors, Rock)= 6
scorePoints _ = 0

scoreGame (move1, move2) =  scorePoints (move1, move2) + (scoreMove move2 )

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let games = map  (encodeGame . (splitOn ' ')) $ lines contents
  let scoredGames = map scoreGame games
  print (sum scoredGames)
  hClose handle
  
