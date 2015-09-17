{-
 - Strategy (AI) implemented using the negamax algorithm.
 - -}

module Strategy.Negamax
(
    negamaxStrategy
) where

import Control.Lens
import Data.List (maximumBy, zipWith4)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Tree

import Board
import GameState
import Strategy.Evaluation (evaluate)

data GameNode = GN { gnBoard  :: Board , gnColumn :: Int,
                     gnPlayer :: Player, gnDepth  :: Int }

genGameTree :: Int -> GameNode -> Tree GameNode
genGameTree maxDepth = unfoldTree generatingFun
    where generatingFun node@(GN board _ player depth) =
            let genNode col = do
                    newBoard <- putPiece player col board
                    return (newBoard, col)
                (possiblePlays, columns) = unzip $ mapMaybe genNode
                                                   [0..boardCols-1]
                newDepth = depth + 1
                childNodes = if newDepth > maxDepth
                                then []
                                else zipWith4 GN possiblePlays
                                                 columns
                                                 (repeat $ nextPlayer player)
                                                 (repeat newDepth)
            in  (node, childNodes)

evalGameTree :: Tree GameNode -> Int
evalGameTree (Node _ childTrees) =
        gnColumn . snd . maximumBy (comparing fst) .
        map (\subt -> (-(evalGameTree' subt), rootLabel subt)) $ childTrees
    where evalGameTree' (Node (GN board _ pl _) []) =
            evaluate pl 1 (-1) board
          evalGameTree' (Node _ childTrees) =
            maximum . map (negate . evalGameTree') $ childTrees

-- |The exported strategy.
negamaxStrategy :: Int -> GameStrategy
negamaxStrategy maxDepth = do
    player <- view gsCurrentPlayer
    curBoard <- view gsBoard

    return . evalGameTree $ genGameTree maxDepth (GN curBoard 0 player 0)
