import Data.Binary.Get (isEmpty)
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat pos str = do
    goto pos
    putStr str

width :: Int
width = 100

height :: Int
height = 30

type Board = [Pos]

showcells :: Board -> IO ()
showcells [] = do
    cls
    goto (0,0)
showcells board = do
    cls
    sequence_ [writeat (x+1,y+1) "x" | (x,y) <- board]
    goto (0, height+1)

isAlive :: Board -> Pos -> Bool
isAlive board pos = 1 == length (filter (==pos) board)

isEmpty :: Board -> Pos -> Bool
isEmpty board pos = not (isAlive board pos)

neighbours :: Pos -> [Pos]
neighbours (x,y) = [
    (inmod (x-1) width, inmod y height),
    (inmod (x+1) width, inmod y height),
    (inmod x width, inmod (y-1) height),
    (inmod x width, inmod (y+1) height),
    (inmod (x+1) width, inmod (y+1) height),
    (inmod (x-1) width, inmod (y-1) height),
    (inmod (x+1) width, inmod (y-1) height),
    (inmod (x-1) width, inmod (y+1) height)
    ]


inmod :: Integral p => p -> p -> p
inmod num lengths
    | num < 0 = inmod (num + lengths) lengths
    | otherwise = num `mod` lengths

livingneighbours :: Board -> Pos -> Int
livingneighbours board pos = length [c1 | c1 <- neighbours pos, c2 <- board, c1 == c2]

survivors :: Board -> Board
survivors board = [c | c <- board, count <- [livingneighbours board c], count > 1 && count < 4]

removeDuplicates :: Board -> Board
removeDuplicates board = [ a | i <- [0 .. length board - 1], a <- [board !! i], a `notElem` drop (i+1) board]

births :: Board -> Board
births board = removeDuplicates [ n | c <- board, n <- neighbours c, livingneighbours board n == 3]

nextgen :: Board -> Board
nextgen board = removeDuplicates (survivors board ++ births board)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n*1000] ]

life :: Board -> IO ()
life board = do
    showcells board
    wait 100
    life (nextgen board)





