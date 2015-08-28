{-# LANGUAGE TemplateHaskell #-}

{- Conect4 implementation in Haskell.
 - NOTE: I know the comments in this file are a bit much extense. This has been
 - on purpose, so in case someone stumbles upon this source code and wants
 - to learn Haskell, they can somewhat understand what is going on (while
 - helping myself remember the concepts I have learned about Haskell).
-}

import Control.Lens
import Control.Monad (join, guard)
import Control.Monad.State
import Data.Vector ((//), (!), (!?))
import qualified Data.Vector as V
import Data.Maybe (isNothing, isJust)
import Data.List (group, find)
import Safe (headMay, lastMay, readMay)
import System.Random

-- |The 'Piece' data structure represents both the pieces in the board
-- and the players themselves as X and O.
data Piece = X | O deriving (Show, Eq)
-- |Returns the next player that plays after another given as an argument.
nextPlayer :: Piece -> Piece
nextPlayer O = X
nextPlayer X = O

-- |Instance declaration of Piece for the Random typeclass. This makes possible
-- the random generation of pieces using the functions in the System.Random
-- module.
instance Random Piece where
    random g =
        let (i, newGen) = randomR (0 :: Int, 1 :: Int) g
            randomPiece = if i == 0 then X else O
        in  (randomPiece, newGen)
    randomR _ = random

-- |A square in the board. A square may be empty (no piece placed) or non-empty.
type Square = Maybe Piece

-- |A row of squares on a board.
type Row = V.Vector Square

-- |The board is represented as a list of rows.
type Board = V.Vector Row

-- |Constant representing the number of rows in the board.
boardRows :: Int
boardRows = 6

-- |Constant representing the number of columns in the board.
boardCols :: Int
boardCols = 7

-- |The empty board (no pieces).
emptyBoard :: Board
emptyBoard = V.replicate boardRows $ V.replicate boardCols Nothing

-- Board access functions
-- |Returns the square at the specified row and column in the board given as
-- argument.
rowIdx :: Board -> Int -> Int -> Square
rowIdx board row col = board ! row ! col

-- |Same as 'rowIdx', but with the position of the row and column arguments
-- switched.
colIdx :: Board -> Int -> Int -> Square
colIdx board = flip $ rowIdx board

-- |Checks if row/col combination is valid (i.e: whether it's within the bounds
-- of the board).
isCoordValid :: (Int, Int) -> Bool
isCoordValid (row, col) = row >= 0 && row < boardRows && col >= 0 && col < boardCols

-- Board transform functions
-- |Try to place a piece in a column of the board. If the column is not valid
-- (i.e: not within the bounds) or the column is full, returns 'Nothing'.
-- Otherwise, returns the resulting board.
putPiece :: Piece -> Int -> Board -> Maybe Board
putPiece piece col board = do
    -- Discard the computation right away if the column is out of bounds by
    -- returning a 'Nothing' if it is the case, using the 'guard' function
    -- declared in the MonadPlus class. Because Maybe is an instance of this
    -- class, we can use 'guard'.
    guard (0 <= col && col < boardCols)

    -- Find the row where the piece would fall when inserted in the column. To
    -- do so, we take the list of cells of the column (from top to bottom) and
    -- see what is the last empty square. This square is where the new piece
    -- will be located, and here we try to get the row where it is.
    -- This is a monadic (Maybe) action because the row might be full already,
    -- therefore not having room for the next piece (in this case, a Nothing is
    -- returned and the computation fails).
    rowIdx <- fmap fst . lastMay . takeWhile (isNothing . snd) $
                colCellsWithIndex

    -- The (!?) infix function returns a Maybe object: a 'Nothing' if the index
    -- is not valid and a 'Just x' otherwise. In this case, the index is
    -- guaranteed to be valid but this operator helps us keeping the returned
    -- value in a Maybe context.
    oldRow <- board !? rowIdx

    -- Get the new row of the board by putting the new piece in it.
    let newRow = oldRow // [(col, Just piece)]
    -- Replace the row in the board and return the new board (in this context,
    -- 'return' means putting it in a Maybe context).
    return $ board // [(rowIdx, newRow)]
    where -- Grab the cells of a columns along with the row where they belong.
          colCellsWithIndex = map
                           (\row -> (row, colIdx board col row))
                           [0..boardRows-1]

-- |Search for four pieces of the same kind in a row in a vector of squares.
fourInARow :: V.Vector Square -> Maybe Piece
fourInARow = -- Easier to convert to list so we can use pattern-matching.
             join . checkFour . V.toList
    where checkFour (x:y:z:t:l) | all isJust [x, y, z, t] && x == y
                                                && y == z && z == t = Just x
                                | otherwise = checkFour (y:z:t:l)
          checkFour _ = Nothing

-- |Returns a vector from the columns of the given board (most technically,
-- what this function does is transpose the board).
extractCols :: Board -> V.Vector Row
extractCols board = do
    -- Take each possible column index.
    col <- V.fromList [0..boardCols-1]
    -- And return the column vector. Notice that Vector as a monad works the
    -- same way as the List monad. This means that this action will be evaluated
    -- for all possible values of "col", and the returned Vector will combine
    -- all of these results.
    return $ V.map (!col) board

-- |Return a vector of the diagonals of the given board.
extractDiagonals :: Board -> V.Vector Row
extractDiagonals board =
    let -- Get vectors of coordinates of diagonals
        coords = genDiagDCoords ++ genDiagUCoords
        -- Convert coordinates to squares from the board
        rowList = (map . map) (uncurry $ rowIdx board) coords
    in  -- Convert list of lists to vector of vectors.
        V.fromList (map V.fromList $ rowList)
    where genDiagDCoords = -- Generate coords for \ diagonals.
            map (takeWhile isCoordValid . iterate (dMap (succ, pred)))
                (topCoords ++ leftCoords)
          genDiagUCoords = -- Generate coords for / diagonals.
            map (takeWhile isCoordValid . iterate (dMap (pred, succ)))
                (botCoords ++ leftCoords)
          -- Coordinates of the top row of the board.
          topCoords = repeat 0 `zip` [0..boardCols-1]
          -- Coordinates of the left column of the board.
          leftCoords = [1..boardRows-1] `zip` repeat 0
          -- Coordinates of the bottom row of the board.
          botCoords = repeat (boardRows-1) `zip` [0..boardCols-1]
          -- Auxiliary function used to generate diagonals.
          dMap (f, g) (x, y) = (f x, g y)

-- |Return the winner of a game by looking at the board.
checkWinner :: Board -> Maybe Piece
checkWinner b = -- Checks for four in a row in rows, columns and diagonals.
                -- "join" merges two nested redundant monads into one.
                -- In this case, converts Maybe (Maybe Piece) into Maybe Piece.
                join . find isJust . V.map fourInARow $
                V.concat [b, extractCols b, extractDiagonals b]

-- |Checks if the board is full (thus no more plays are possible).
isFull :: Board -> Bool
isFull = -- Check if the first (top) row is full.
         V.all isJust . V.head

-- Printing functions
-- |Prints the contents of a row to stdout ('.' represents an empty cell).
printRow :: Row -> IO ()
printRow row = -- Map the monadic action of "printing" to each square of the
               -- row. Then print a new line.
               V.mapM_ (putChar . maybe '.' (head . show)) row >> putStrLn ""

-- |Prints a board to stdout.
printBoard :: Board -> IO ()
printBoard = V.mapM_ printRow

-- Game input/output functions
-- |Represents the state of the program at a particular time.
data GameState = GameState { _gsBoard :: Board, _gsStdGen :: StdGen,
                             _gsCurrentPlayer :: Piece }
-- Construct the lenses for the previously defined data structure. In layman
-- terms, lenses are abstractions used in functional programming to simulate
-- getters and setters in a composable, functional and convenient way. The
-- following line makes use of the "TemplateHaskell" extension, which acts
-- like some kind of preprocessor, thus generating the code for the lenses at
-- compile time (as opposed to at runtime). This way we can use the lenses
-- like any other defined object in the rest of our program.
-- The created lenses will have the same name as the corresponding field,
-- but without the leading underscore.
makeLenses ''GameState

-- |Reads a column number from stdin.
readCol :: IO (Maybe Int)
readCol = getLine >>= return . readMay

-- |Prints whose turn is it.
printTurn :: StateT GameState IO ()
printTurn = do
    cur <- use gsCurrentPlayer
    liftIO . putStrLn $ "It is the turn for " ++ (show cur)

-- |Play a turn for a given player (i.e: read column and place piece).
playTurn :: StateT GameState IO ()
playTurn = do
    liftIO . putStrLn $ "Enter a valid column number (0.." ++ (show $ boardCols-1)
                                                           ++ ")"
    -- The 'lens' package provides some functions analogous to the ones defined
    -- in the state monad. For example: 'use' is like 'gets', but the former
    -- lets us use a lens as a getter, while the later needs a explicit
    -- function over the state.
    currentPlayer <- use gsCurrentPlayer
    curBoard <- use gsBoard

    -- Read column.
    mcol <- liftIO readCol
    -- Nested do block. Needed because we have to run several actions
    -- corresponding to the Maybe monad and not to the external one (StateT).
    -- Here, we check the existence of the column and thst the resulting board
    -- is valid (returning the resulting board if that's the case, or Nothing
    -- otherwise).
    let mboard = do
            acol <- mcol
            putPiece currentPlayer acol curBoard
    -- Check validity of the board, returning it if it's valid, or asking
    -- again for the column if it's not the case.
    verifyBoard mboard
    where verifyBoard Nothing = playTurn  -- If board is not valid, ask again
          verifyBoard (Just newBoard) = do
            assign gsBoard newBoard
            gsCurrentPlayer %= nextPlayer

-- |Function called when the game ends. Prints final board and winner.
gameOver :: Maybe Piece -> StateT GameState IO ()
gameOver winner = do
    finalBoard <- use gsBoard
    liftIO $ do
        putStrLn ""
        printBoard finalBoard
        putStrLn $ "The game is over! " ++
            (maybe "Tie!" (\w -> (show w) ++ " wins!") winner)

-- |Data type that represents the state of a match.
data MatchState = NotEnd | Win Piece | Tie
checkEnd :: StateT GameState IO MatchState
checkEnd = do
    board <- use gsBoard
    return $ case checkWinner board of
        Nothing     -> if isFull board then Tie else NotEnd
        Just winner -> Win winner

-- |Loop of the game. This function repeats itself until there is a winner
-- (or no more plays are possible).
mainLoop :: StateT GameState IO ()
mainLoop = do
    board <- use gsBoard
    liftIO $ putStrLn ""
    liftIO $ printBoard board
    printTurn
    playTurn
    matchSt <- checkEnd
    case matchSt of
        NotEnd -> mainLoop
        Win w  -> gameOver $ Just w
        Tie    -> gameOver Nothing

-- |Main function, entry point to the application.
main :: IO ()
main = do
    stdGen <- getStdGen
    let (initialPlayer, newGen) = random stdGen
        initialState = GameState emptyBoard newGen initialPlayer
    mainLoop `runStateT` initialState
    return ()
