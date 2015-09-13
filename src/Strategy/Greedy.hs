{-
 - This file defines the strategy for a player controller by the computer.
 - -}

module Strategy.Greedy
(
    computerStrategy
) where

import Control.Lens (view)
import Control.Monad (join)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V

import Board
import GameState

-- |Auxiliary data type to represent the result of the evaluation function 
newtype NumPair a = NumPair { tuple :: (a, a) }

-- |Adds two 'NumPair' objects by adding the components of the wrapped tuples.
plus :: Num a => NumPair a -> NumPair a -> NumPair a
(NumPair (a, b)) `plus` (NumPair (c, d)) = NumPair (a+c, b+d)

-- |The zero pair. Just a pair with zeroes in it.
zeroPair :: Num a => NumPair a
zeroPair = NumPair (0, 0)

-- Now, this may seem weird at first but... the 'NumPair' type we defined is
-- a 'Monoid'. Why? Because it has an associative binary operation ('plus')
-- and an identity element for that operation ('zeroPair'). Monoids have
-- the nice feature of being 'concatable' things, i.e. if you have a list
-- of objects from a particular Monoid type, you can easily reduce them to
-- a single element (and this is done by repeatedly using the 'mappend'
-- function). Declaring this instance will tell Haskell that 'NumPair a' is
-- a monoid, and this will simplify things later.
instance Num a => Monoid (NumPair a) where
    mempty = zeroPair
    mappend = plus

-- |Evaluates the "goodness" of a board for a given player.
-- This function can be paraphrased as "the number of combinations of 4 adjacent
-- squares* that have at least one piece of the player, and none of the
-- adversary."
-- * Here, "adjacent" means that both squares are next to each other
--   horizontally, vertically or diagonally.
-- Returns a pair containing the evaluations for the given player and their
-- adversary, respectively.
winningRows :: Player -> Board -> (Int, Int)
winningRows pl board = tuple . winningRows' $ combs
    where -- 'combs' is just all the rows, columns and diagonals of the board.
          combs = join $ (V.toList.) <$> [id, extractCols, extractDiagonals]
                                     <*> pure board
          -- 'foldMap' is a neat function that does these two steps:
          -- 1. Maps the given function (1st argument) over the list (2nd arg)
          --    Said function MUST return a particular monoid type.
          -- 2. When step 1 is done, we have a list of monoids that we fold
          --    (reduce) into a single element, as explained above.
          -- TL;DR: winningRows' takes a list of rows, maps 'countRows' (which
          --  returns 'NumPair' values) and sums the results together.
          winningRows' = foldMap countRow
          -- Takes a row and gives it a score given by the 'countFour' function.
          countRow row
            | V.length row < 4 = zeroPair
            | otherwise = countFour (V.take 4 row) `plus` countRow (V.tail row)
          -- 'countFour' takes each possible 4-adjacent piece combination and
          -- values it depending of how many pieces of each player are there.
          countFour f =
            let myPieceCount       = howMany pl f
                opponentPieceCount = howMany (nextPlayer pl) f
            in  case (myPieceCount, opponentPieceCount) of
                    -- If there are four of a kind, the game is won so we give
                    -- these combinations a huge score.
                    (4, 0) -> NumPair (10000, 0)
                    (0, 4) -> NumPair (0, 10000)
                    -- Zero of each kind favours neither player.
                    (0, 0) -> zeroPair
                    -- More pieces = more value/risk. Promotes creating longer
                    -- chains.
                    (a, 0) -> NumPair (a, 0)
                    (0, b) -> NumPair (0, b)
                    -- Anything else (i.e. there are pieces of both players)
                    -- are not valuable to neither player.
                    (_, _) -> zeroPair
          -- Count the number of pieces of a given player in a vector.
          howMany pi = V.length . V.filter (== Just pi)

-- |The heuristic function, as pointed in the paper from
-- http://www.ics.uci.edu/~jlam2/connectk.pdf,
-- should be a linear combination of the 'winningRows' function calculated
-- for each of the players.
-- The 'Int' parameters to this function are the coefficients for said linear
-- combination.
evaluate :: Piece -> Int -> Int -> Board -> Int
evaluate pl plCoef opCoef board =
    case winningRows pl board of
        (winPl, winOp) -> plCoef * winPl + opCoef * winOp

-- |Data type that contains information about each possible move to make.
-- This info is: 1. the column we inserted the piece into, 2. the resulting
-- board and 3. the rating given by the evaluation/heuristic function.
data Move = Move { mvColumn :: Int, mvResult :: Board, mvRating :: Int }

-- |The exported strategy. Determines (greedily) the best move to make.
computerStrategy :: GameStrategy
computerStrategy = do
    player <- view gsCurrentPlayer
    curBoard <- view gsBoard

    -- For each column, we try putting a piece (we discard invalid movements
    -- thanks to the 'mapMaybe' function), and then get the column for which
    -- we got the maximum rating.
    return . mvColumn . maximumBy (compare `on` mvRating) .
        mapMaybe (move player curBoard) $ [0..boardCols-1]
    where -- Creates the 'Move' data structure for a given column.
          move player board col = 
            case putPiece player col board of
                Nothing -> Nothing
                Just resultBoard -> Just $ Move col resultBoard
                    (evaluate player playerCoef opponentCoef resultBoard)
          -- Coefficients for the linear combination of winning rows. We
          -- temporarily use a greater weight for the opponent's so we can get
          -- a more aggresive AI that doesn't lose so easily. This may change
          -- later.
          playerCoef   = 1
          opponentCoef = -3
