{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad (join, guard)
import Control.Monad.State
import Data.Vector ((//), (!), (!?))
import qualified Data.Vector as V
import Data.Maybe (isNothing, isJust)
import Data.List (group, find)
import Safe (headMay, lastMay, readMay)
import System.Random

data Piece = X | O deriving (Show, Eq)
nextPlayer :: Piece -> Piece
nextPlayer O = X
nextPlayer X = O

instance Random Piece where
    random g =
        let (i, newGen) = randomR (0 :: Int, 1 :: Int) g
            randomPiece = if i == 0 then X else O
        in  (randomPiece, newGen)
    randomR _ = random

type Square = Maybe Piece
type Row = V.Vector Square
type Board = V.Vector Row
type Coord = (Int, Int)

boardRows :: Int
boardRows = 6

boardCols :: Int
boardCols = 7

emptyBoard :: Board
emptyBoard = V.replicate boardRows $ V.replicate boardCols Nothing

-- Board access functions
rowIdx :: Board -> Int -> Int -> Square
rowIdx board row col = board ! row ! col

colIdx :: Board -> Int -> Int -> Square
colIdx board = flip $ rowIdx board

-- Check if row/col combination is valid
isCoordValid :: (Int, Int) -> Bool
isCoordValid (row, col) = row >= 0 && row < boardRows && col >= 0 && col < boardCols

-- Board transform functions
-- Maybe refactor this to State monad?
putPiece :: Piece -> Int -> Board -> Maybe Board
putPiece piece col board = do
    guard (0 <= col && col < boardCols)
    rowIdx <- fmap fst . lastMay . takeWhile (isNothing . snd) $ colCellsWithIndex
    oldRow <- board !? rowIdx
    let newRow = oldRow // [(col, Just piece)]
    return $ board // [(rowIdx, newRow)]
    where colCellsWithIndex = map
                           (\row -> (row, colIdx board col row))
                           [0..boardRows-1]

-- Check for win state
fourInARow :: V.Vector Square -> Maybe Piece
fourInARow = join . headMay . filter isJust . map head . filter ((>=4) . length) . group . V.toList

extractCols :: Board -> V.Vector Row
extractCols board = do
    col <- [0..boardCols-1]
    return $ V.map (!col) board

extractDiagonals :: Board -> V.Vector Row
extractDiagonals board =
    let coords = genDiagDCoords ++ genDiagUCoords
        rowList = (map . map) (uncurry $ rowIdx board) coords
    in  V.fromList (map V.fromList $ rowList)
    where genDiagDCoords =
            map (takeWhile isCoordValid . iterate (dMap (succ, pred))) (topCoords ++ leftCoords)
          genDiagUCoords =
            map (takeWhile isCoordValid . iterate (dMap (pred, succ))) (botCoords ++ leftCoords)
          topCoords = repeat 0 `zip` [0..boardCols-1]
          leftCoords = [1..boardRows-1] `zip` repeat 0
          botCoords = repeat (boardRows-1) `zip` [0..boardCols-1]
          dMap (f, g) (x, y) = (f x, g y)

checkWinner :: Board -> Maybe Piece
checkWinner b = join . find isJust . V.map fourInARow $ V.concat [b, extractCols b, extractDiagonals b]

-- Printing functions
printRow :: Row -> IO ()
printRow row = V.mapM_ (putChar . maybe '.' (head . show)) row >> putStrLn ""

printBoard :: Board -> IO ()
printBoard = V.mapM_ printRow

-- Game input/output functions
data GameState = GameState { _gsBoard :: Board, _gsStdGen :: StdGen, _gsCurrentPlayer :: Piece }
makeLenses ''GameState

getWinner :: StateT GameState IO (Maybe Piece)
getWinner = use gsBoard >>= return . checkWinner

readCol :: IO (Maybe Int)
readCol = getLine >>= return . readMay

printTurn :: StateT GameState IO ()
printTurn = do
    cur <- use gsCurrentPlayer
    liftIO . putStrLn $ "It is the turn for " ++ (show cur)

playTurn :: StateT GameState IO ()
playTurn = do
    liftIO . putStrLn $ "Enter a valid column number (0.." ++ (show boardCols) ++ ")"
    currentPlayer <- use gsCurrentPlayer
    curBoard <- use gsBoard
    mcol <- liftIO readCol
    let mboard = do
            acol <- mcol
            putPiece currentPlayer acol curBoard
    verifyBoard mboard
    where verifyBoard Nothing = playTurn  -- If board is not valid, then ask again
          verifyBoard (Just newBoard) = do
            assign gsBoard newBoard
            gsCurrentPlayer %= nextPlayer

gameOver :: Piece -> StateT GameState IO ()
gameOver winner = liftIO . putStrLn $ "The game is over! " ++ (show winner) ++ " wins!"

mainLoop :: StateT GameState IO ()
mainLoop = do
    board <- use gsBoard
    liftIO $ printBoard board
    printTurn
    playTurn
    mwinner <- getWinner
    case mwinner of
        Nothing -> mainLoop
        Just w -> gameOver w

main :: IO ()
main = do
    stdGen <- getStdGen
    let (initialPlayer, newGen) = random stdGen
        initialState = GameState emptyBoard newGen initialPlayer
    mainLoop `runStateT` initialState
    return ()
