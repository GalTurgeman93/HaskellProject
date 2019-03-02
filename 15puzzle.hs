import Prelude hiding (Right, Left)
import Data.Maybe (mapMaybe)

example = [[1,2,3, 4] ,[5,6,7,8],[9, 10, 11, 12], [13,14,15,0]]
example2 =  [[6,9,7,4], [2,5,10,8], [3,11,1,12], [13,14,15,0]]
example3 = [[2,3,7,4], [1,6,11,8],[5,10,0,12], [9,13,14,15]]
example_new = [[0,1,2,3], [5,6,7,4], [9,10,11,8], [13,14,15,12]]


---------------------------------------------------- Definitions -----------------------------------------

data Row a = Empty | Val a (Row a) deriving (Show, Ord, Eq)
type Board = [Row Int]
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

--------------------------------------------- Defining Instances ---------------------------------------------

instance Functor Row where  
    fmap f Empty = Empty
    fmap f (Val x xs) = Val (f x) (fmap f xs)

instance Applicative Row where
    pure a = Val a Empty
    (<*>) Empty _ = Empty
    (<*>) _ Empty = Empty
    (<*>) (Val f next) (Val a next') = Val (f a) (next <*> next')

instance Monad Row where
    return a = pure a
    (>>=) Empty f = Empty
    (>>=) (Val x next) f = Val (res) ((>>=) next f)
              where
                (Val res next') = f x

-------------------------------------------- Building the Components -----------------------------------------

createBoard :: [[Int]] -> Position -> Board
createBoard [] _ = []
createBoard (x:xs) (p1,p2) = [createRow x (p1,p2)] ++ createBoard xs (p1+1, p2)

createRow :: [Int] -> Position -> Row Int
createRow [] _ = Empty
createRow (x:xs) (p1,p2) = Val x (createRow xs (p1, p2+1))

createPuzzle :: [[Int]] -> Position -> Puzzle
createPuzzle xs blankPos = Puzzle board dist dim blank moves Nothing
  where
    board = createBoard xs (0,0)
    blank = blankPos
    dim = length xs
    dist = getDist (boardDist board (0,0) (length xs))
    moves = 0

----------------------------------------------- Defining Heuirstic -------------------------------------------

getDist :: Row Int -> Int
getDist (Val v next) = v

boardDist :: Board -> Position -> Int -> Row Int
boardDist [] _ _ = Val 0 Empty
boardDist (x:xs) (p1,p2) dim = ((+) <$> (rowDist x (p1,p2) dim) <*> (boardDist xs (p1+1, p2) dim))

rowDist :: Row Int -> Position -> Int -> Row Int 
rowDist Empty _ _ = Val 0 Empty
rowDist (Val v next) (p1, p2) dim = ((+) <$> ((>>=) (Val v next) (manhattanDist (p1,p2) dim)) <*> (rowDist next (p1, p2+1) dim))

manhattanDist :: Position -> Int -> Int -> Row Int
manhattanDist (pos1, pos2) dim value = if value == 0 then (Val 0 Empty) else result
    where
        --v = fromInt value
        rowDist = abs (pos1 - ((value-1) `div` dim))
        colDist = abs (pos2 - ((value-1) `mod` dim))
        result = Val (rowDist + colDist) Empty

----------------------------------------------- Updating after a move -----------------------------------------

-- returns the tile value at the given position
getTile :: Board -> Position -> Int
getTile (x:xs) (0, pos2) = getTile' x (0, pos2)
getTile (x:xs) (pos1, pos2) = getTile xs (pos1 - 1, pos2)

-- returns the tile at the given position
getTile' :: Row Int -> Position -> Int
getTile' Empty (pos1, pos2) = -1
getTile' (Val x row) (pos1, 0) = x
getTile' (Val x row) (pos1, pos2) = getTile' row (pos1, pos2-1)

--set value in the given position
setBoard :: Board -> Position -> Int -> Board
setBoard [] _ _ = []  
setBoard (x:xs) (0,pos2) newVal = [(setRow x (0,pos2) newVal)] ++ xs
setBoard (x:xs) (pos1,pos2) newVal = [x] ++ (setBoard xs (pos1-1,pos2) newVal)

setRow :: Row Int -> Position -> Int -> Row Int
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
                 , dist = getDist (boardDist newBoard (0,0) (dim p))
                 , moves = moves p + 1
                 , previous = Just p 
             }
        where
            board' = board p
            blank' = blank p
            newBoard = swapTiles board' blank' pos

---------------------------------------------- Solve Process -----------------------------------------------            

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
        | (priority > xpriority) = (xpriority,xpuzzle) : (insertPuzzle (priority,puzzle) xs)
        | otherwise = (priority,puzzle) : (xpriority,xpuzzle) : xs

removeMin :: MovesQueue -> MovesQueue
removeMin queueMoves = tail queueMoves

insertQueue :: [(Int, Puzzle)] -> MovesQueue -> MovesQueue
insertQueue [] q = q
insertQueue (x:xs) q = insertQueue xs (insertPuzzle x q)

solve :: MovesQueue -> Puzzle
solve queueMoves =  if dist puzzle == 0 
                    then puzzle 
                    else solve newQueueMoves
                    where
                        (_,puzzle) = head queueMoves
                        ns = case (previous puzzle) of
                                    Nothing -> neighbors puzzle
                                    Just n  -> filter (\x -> board x /= board n) (neighbors puzzle)
                        ps  = zip [moves q + dist q | q <- ns] ns
                        queueMoves' = removeMin queueMoves
                        newQueueMoves = insertQueue ps queueMoves'

   
-------------------------------------------------- Print ------------------------------------------------

--print board
printBoard :: Board -> IO()
printBoard [] = putStrLn $ show ""
printBoard (x:xs) = do 
                    putStrLn $ show (rowToString x)
                    printBoard xs

rowToString:: Row Int -> String
rowToString Empty = ""
rowToString (Val 0 row) = "_" ++ (rowToString row)
rowToString (Val x row) = (show x) ++  " " ++ (rowToString row)

-------------------------------------------------- Main -------------------------------------------------

-- solve' :: MovesQueue -> IO()
-- solve' queueMoves =  do
                    -- print (head queueMoves)
                    -- solve' (tail queueMoves)

solve' :: MovesQueue -> IO()
solve' queueMoves = do 
                    print (map fst queueMoves)
                    -- print (dist (snd (head queueMoves)))
                    -- printBoard (board (snd (head queueMoves)))
                    print (" ============================")
                    if dist puzzle == 0 
                    then print ""
                    else solve' newQueueMoves
                    where
                        (_,puzzle) = head queueMoves
                        ns = case (previous puzzle) of
                                    Nothing -> neighbors puzzle
                                    Just n  -> filter (\x -> board x /= board n) (neighbors puzzle)
                        ps  = zip [moves q + dist q | q <- ns] ns
                        queueMoves' = removeMin queueMoves
                        newQueueMoves = insertQueue ps queueMoves'

checkSol :: Position -> [[Int]] -> IO()
checkSol pos matrix =  do 
	let result = solve queue
		where 
			puzzle = createPuzzle matrix pos
			queue = [((dist puzzle),puzzle)]
	printBoard (board result)


fromString :: String -> [[Int]]
fromString s = (map . map) read ws
  where ws = map words (lines s)

main = do
    putStrLn "Enter the name of the file containing the puzzle:"
    txt <- readFile =<< getLine
    let game = fromString txt
        ([p1,p2], brd) = case game of
          [] -> error "Invalid puzzle file"
          x:xs -> (x, xs)
	
    checkSol (p1,p2) brd
