import Prelude hiding (Left, Right)
import Data.List
import System.Random

data Move = Up | Down | Left | Right
type Grid = [[Int]]   




main :: IO ()
main = gameLoop start

gameLoop :: Grid -> IO ()