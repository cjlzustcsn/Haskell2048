import Data.List
import System.Random

data Move = U | D | L | R
type Grid = [[Int]]   

main :: IO ()
main = gameLoop start

gameLoop :: Grid -> IO ()
gameLoop grid =
    case isMoveable grid of
            True  -> do printGrid grid
                        if checkwin grid
                        then putStrLn "You won!"
                        else do new_grid <- newGrid grid                        
                                if grid /= new_grid
                                then do new <- addTile new_grid
                                        gameLoop new
                                else gameLoop grid
            False -> do printGrid grid
                        putStrLn "Game over"

isMoveable :: Grid -> Bool
isMoveable grid = sum possibilities > 0
    where possibilities = map(length . getZeroes . move grid) [L, R, U, D]

getZeroes :: Grid -> [(Int, Int)]
getZeroes grid = filter (\(row, col) -> (grid!!row)!!col == 0) coordinates
    where singleRow n = zip (replicate 4 n)[0..3]
          coordinates = concatMap singleRow [0..3]

move :: Grid -> Move -> Grid
move grid L  = map merge grid
move grid R = map (reverse . merge . reverse) grid
move grid U    = transpose $ move (transpose grid) L
move grid D  = transpose $ move (transpose grid) R

merge :: [Int] -> [Int]
merge xs = merged ++ padding
    where padding          = replicate (length xs - length merged) 0
          merged           = combine $ filter (/= 0) xs
          combine (x:y:xs) | x == y    = x * 2 : combine xs
                           | otherwise = x     : combine (y:xs) 
          combine x        = x

printGrid :: Grid -> IO ()
printGrid grid = do putStr $ "\ESC[2J" ++ "\ESC[2J" --- clears the screen
                    mapM_ (putStrLn . printRow) grid

printRow :: [Int] -> String
printRow (x:xs) = front ++ show x ++ printRow xs
    where front = concat $ replicate (5 - length (show x)) " "  
printRow []     = ""

checkwin :: Grid -> Bool
checkwin grid = [] /= filter (== 512) (concat grid)

newGrid :: Grid -> IO Grid
newGrid grid = do m <- captureMove    
                  let new_grid = move grid m                         
                  return new_grid

captureMove :: IO Move    
captureMove = do 
    inp <- getLine
    case inp of
        "w" -> return U
        "a" -> return L
        "s" -> return D
        "d" -> return R
        _   -> do putStrLn "Use WASD (lowercase) as input"
                  captureMove 

addTile :: Grid -> IO Grid
addTile grid = do g <- newStdGen
                  let candidates      = getZeroes grid
                      pos             = head (randoms g :: [Int]) `mod` length candidates
                      pick            = candidates!!pos
                      val             = [2,2,2,2,2,2,2,2,2,4] !! (head (randoms g :: [Int]) `mod` 10)
                      new_grid        = setSquare grid pick val
                  return new_grid

setSquare :: Grid -> (Int, Int) -> Int -> Grid
setSquare grid (row, col) val = pre ++ [mid] ++ post
    where pre  = take row grid
          mid  = take col (grid!!row) ++ [val] ++ drop (col + 1) (grid!!row)
          post = drop (row + 1) grid

start :: Grid
start = [[0, 0, 0, 0],
         [0, 0, 0, 0],
         [0, 0, 0, 2],
         [0, 0, 0, 2]]