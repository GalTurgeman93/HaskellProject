import Prelude hiding (Right, Left)
import Data.Maybe (mapMaybe)

example = [[1,2,3, 4] ,[5,6,7,8],[9, 10, 11, 12], [13,14,15,0]]
---------------------------------------------------- Definitions -----------------------------------------

data Row a = Empty | Val a (Row a) deriving (Show, Ord, Eq)
type Board = [Row Integer]
type Position = (Int, Int) 

data Puzzle = Puzzle 
  { board     :: Board
  , dist      :: Int
  , dim       :: Int 
  , blank     :: Position
  , moves     :: Int
  , previous  :: Maybe Puzzle 
  } deriving (Show, Eq, Ord)

data Direction = Up | Right | Down | Left
type MovesQueue = [(Int,Puzzle)]


-------------------------------------------- Building the Components -----------------------------------------

createBoard :: [[Integer]] -> Position -> Board
createBoard [] _ = []
createBoard (x:xs) (p1,p2) = [createRow x (p1,p2)] ++ createBoard xs (p1+1, p2)

createRow :: [Integer] -> Position -> Row Integer
createRow [] _ = Empty
createRow (x:xs) (p1,p2) = Val x (createRow xs (p1, p2+1))

createPuzzle :: [[Integer]] -> Puzzle
createPuzzle xs = Puzzle board dist dim blank moves Nothing
  where
    board = createBoard xs (0,0)
    blank = (length xs-1, length (head xs)-1) 
    dim = length xs
    dist = boardDist board (0,0) dim
    moves = 0

--------------------------------------------- Defining Instances ---------------------------------------------

instance Functor Row where  
    fmap f Empty = Empty
    fmap f (Val x xs) = Val (f x) (fmap f xs)


----------------------------------------------- Defining Heuirstic -------------------------------------------

manhattanDist :: Integer -> Position -> Int -> Int
manhattanDist value (pos1, pos2) dim = if value == 0 then 0 else result
    where
        v = fromInteger value
        rowDist = abs (pos1-1 - ((v-1) `div` dim))
        colDist = abs (pos2-1 - ((v-1) `mod` dim))
        result = rowDist + colDist

boardDist :: Board -> Position -> Int -> Int
boardDist [] _ _ = 0
boardDist (x:xs) (p1, p2) dim = (rowDist x (p1, p2) dim) + (boardDist xs (p1+1, p2) dim)

rowDist :: Row Integer -> Position -> Int -> Int
rowDist Empty _  _ = 0
rowDist (Val v next) (p1, p2) dim = (manhattanDist v (p1,p2) dim) + (rowDist next (p1, p2+1) dim)

----------------------------------------------- Updating after a move -----------------------------------------

-- returns the tile value at the given position
getTile :: Board -> Position -> Integer
getTile (x:xs) (0, pos2) = getTile' x (0, pos2)
getTile (x:xs) (pos1, pos2) = getTile xs (pos1 - 1, pos2)

-- returns the tile at the given position
getTile' :: Row Integer -> Position -> Integer
getTile' Empty (pos1, pos2) = -1
getTile' (Val x row) (pos1, 0) = x
getTile' (Val x row) (pos1, pos2) = getTile' row (pos1, pos2-1)

--set value in the given position
setBoard :: Board -> Position -> Integer -> Board
setBoard [] _ _ = []  
setBoard (x:xs) (0,pos2) newVal = [(setRow x (0,pos2) newVal)] ++ xs
setBoard (x:xs) (pos1,pos2) newVal = [x] ++ (setBoard xs (pos1-1,pos2) newVal)

setRow :: Row Integer -> Position -> Integer -> Row Integer
setRow Empty _ _ = Empty
setRow (Val y row) (pos1,0) x = (Val x row)
setRow (Val y row) (pos1,pos2) x = (Val y (setRow row (pos1,pos2-1) x))


-- swap tiles at pos1 and pos 2 on board
swapTiles :: Board -> Position -> Position -> Board
swapTiles board pos1 pos2 = setBoard (setBoard board pos2 val1) pos1 val2
    where
        val1 = getTile board pos1
        val2 = getTile board pos2

-- update the puzzle structure after the board changed by swap tiles
update :: Puzzle -> Position -> Puzzle
update p pos = p { board = newBoard
                 , blank = pos
                 , dim = dim p
                 , dist = boardDist newBoard (0,0) (dim p)
                 , moves = moves p + 1
                 , previous = Just p 
             }
        where
            board' = board p
            blank' = blank p
            newBoard = swapTiles board' blank' pos

-- return the board that can be reached from the current board by moving in the given direction
neighbors :: Puzzle -> [Puzzle]
neighbors p = mapMaybe (neighbor p) [Up, Right, Down, Left]

neighbor :: Puzzle -> Direction -> Maybe Puzzle
neighbor p dir = case dir of
    Up -> if i <= 0   then Nothing else Just $ update p ((i-1), j)
    Right -> if j >= n-1 then Nothing else Just $ update p (i, (j+1))
    Down -> if i >= n-1 then Nothing else Just $ update p ((i+1), j)
    Left -> if j <= 0   then Nothing else Just $ update p (i, (j-1))
    where
        (i, j) = blank p
        n = dim p

--insert to the list of moves (the list is sorted by the priority)
--The priority of a puzzle is the number of moves so far plus the manhattan distance.
insertPuzzle :: (Int,Puzzle) -> MovesQueue -> MovesQueue
insertPuzzle pair [] = [(pair)]
insertPuzzle (priority,puzzle) ((xpriority,xpuzzle):xs) 
        | (priority <= xpriority) = (priority,puzzle) : (xpriority,xpuzzle) : xs
        | otherwise = (xpriority,xpuzzle) : (insertPuzzle (priority,puzzle) xs)

removeMin :: MovesQueue -> MovesQueue
removeMin queueMoves = tail queueMoves


-- solve the givan puzzle
solve :: MovesQueue -> Puzzle
solve queueMoves =  if dist puzzle == 0 
                    then puzzle 
                    else solve newPuzzle
                    where
                        (_,puzzle) = head queueMoves
                        ns = case (previous puzzle) of
                                    Nothing -> neighbors puzzle
                                    Just n  -> filter (\x -> board x /= board n) (neighbors puzzle)
                        ps  = zip [moves q + dist q | q <- ns] ns
                        newQueueMoves = foldr insertPuzzle queueMoves ps
                        newPuzzle = removeMin newQueueMoves

   
-------------------------------------------------- Print ------------------------------------------------

--print board
printBoard :: Board -> IO()
printBoard [] = putStrLn $ show ""
printBoard (x:xs) = do 
                    putStrLn $ show (rowToString x)
                    printBoard xs

rowToString:: Row Integer -> String
rowToString Empty = ""
rowToString (Val 0 row) = "_" ++ (rowToString row)
rowToString (Val x row) = (show x) ++  " " ++ (rowToString row)

-------------------------------------------------- Main -------------------------------------------------

checkSol =  do 
	let result = solve queue
		where 
			puzzle = createPuzzle example
			queue = [((dist puzzle),puzzle)]
	print (dim result)
	printBoard (board result)

main = checkSol
