

-- types and data types
data Row a = Empty | Val a (Row a)   --recursive structure
type Board = [Row Int]
type Position = (Int, Int)

instance Functor Row where  
    fmap f Empty = Empty
    fmap f (Val x xs) = Val (f x) (fmap f xs)

-- returns the tile at the given position
getTile :: Board -> Position -> Int
getTile (x:xs) (0, pos2) = getTile' x (0, pos2)
getTile (x:xs) (pos1, pos2) = getTile xs (pos1 - 1, pos2)

-- returns the tile at the given position
getTile' :: Row Int -> Position -> Int
getTile' Empty (pos1, pos2) = -1
getTile' (Val x row) (pos1, 0) = x
getTile' (Val x row) (pos1, pos2) = getTile' row (pos1, pos2-1)

--check if the position is valid
validPos :: Position -> Bool
validPos (pos1, pos2) = pos1 >= 0 && pos2 >= 0 && pos1 < 4 && pos2 < 4

-- returns the valid movements of a tile
getPossiblePositions :: Position -> [Position]
getPossiblePositions pos = filter validPos (getPossiblePositions' pos)

getPossiblePositions' :: Position -> [Position]
getPossiblePositions' (pos1, pos2) = [(pos1 + 1, pos2), (pos1, pos2 + 1), (pos1 - 1, pos2), (pos1, pos2 - 1)]

--get the empty tile position
getEmpty :: Board -> Position


--set value in the given position
setBoard :: Board -> Position -> Int -> Board
setBoard [] _ _ = []  
setBoard (x:xs) (0,pos2) newVal = [(setRow x (0,pos2) newVal)] ++ xs
setBoard (x:xs) (pos1,pos2) newVal = [x] ++ (setBoard xs (pos1-1,pos2) newVal)

setRow :: Row Int -> Position -> Int -> Row Int
setRow Empty _ _ = Empty
setRow (Val y row) (pos1,0) x = (Val x row)
setRow (Val y row) (pos1,pos2) x = (Val y (setRow row (pos1,pos2-1) x))


-- check if board is in final state
isFinal :: Board -> Bool
isFinal board = (flatBoard board) == finalBoard
    where
        finalBoard = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0]
        
flatBoard :: Board -> [Int]
flatBoard [] = []
flatBoard (x:xs) = (flatRow x) ++ (flatBoard xs)

flatRow :: Row Int -> [Int]
flatRow Empty = []
flatRow (Val x row) = [x] ++ (flatRow row)

-- swap tiles at pos1 and pos 2 on board
swapTiles :: Board -> Position -> Position -> Board
swapTiles board pos1 pos2 = setBoard (setBoard board pos2 val1) pos1 val2
    where
        val1 = getTile board pos1
        val2 = getTile board pos2

--print board
printBoard :: Board -> String
printBoard [] = ""
printBoard (x:xs) = (rowToString x) ++ "\n" ++ printBoard xs

rowToString:: Row Int -> String
rowToString Empty = ""
rowToString (Val 0 row) = "_" ++ (rowToString row)
rowToString (Val x row) = (show x) ++ (rowToString row)






