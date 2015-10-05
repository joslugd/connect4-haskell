{-
 - Strategy (AI) implemented using the negamax algorithm.
 - -}

module Strategy.Negamax
(
    negamaxStrategy
) where

import Control.Lens (view)
import Data.List (maximumBy, sortBy, zipWith4)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Tree

import Board
import GameState
import Strategy.Evaluation (evaluate)

-- |Data structure that contains (1) the state of the board at a given time,
-- (2) the column where a piece has been inserted to reach said state, (3)
-- the next player and (4) the depth of the node in the search tree.
data GameNode = GN { gnBoard  :: Board , gnColumn :: Int,
                     gnPlayer :: Player, gnDepth  :: Int }

-- |Generates the game tree up to a given depth ('maxDepth'). The 'Tree' data
-- structure from the 'Data.Tree' package is lazy, which means that its nodes
-- will be generated on demand as opposed to being generated at once.
-- The alpha-beta pruning algorithm takes advantage of this by not visiting
-- the pruned subtrees.
genGameTree :: Int -> GameNode -> Tree GameNode
genGameTree maxDepth = unfoldTree generatingFun
    where -- Function used to generate the nodes of the tree. The seed value is
          -- of the same type as the node type ('GameNode'). At each step, the
          -- function expands the current node generating its children
          -- except when the node is a child node or it's already at the
          -- maximum depth.
          generatingFun node@(GN board _ player depth) =
            let genNode col = do
                    newBoard <- putPiece player col board
                    return (newBoard, col)
                -- Create a column ordering for the children nodes to be 
                -- generated. Here, we want to prioritise the columns closer
                -- to the center of the board, as these are the ones that are
                -- more likely to be the best choices.
                columnArrangement =
                    sortBy (comparing (\col -> abs $ (boardCols `div` 2) - col))
                           [0..boardCols-1]
                -- Get lists of possible boards and used columns.
                (possiblePlays, columns) = unzip $ mapMaybe genNode
                                                   columnArrangement
                newDepth = depth + 1
                -- Generate childs of current node.
                childNodes = if newDepth > maxDepth
                                then []
                                else zipWith4 GN possiblePlays
                                                 columns
                                                 (repeat $ nextPlayer player)
                                                 (repeat newDepth)
            in  case getMatchState board of
                    Win _  -> (node, [])
                    Tie    -> (node, [])
                    NotEnd -> (node, childNodes)

-- |Eval the game tree using the negamax algorithm, returning the best
-- column selection for the current player.
evalGameTree :: Tree GameNode -> Int
evalGameTree (Node _ childTrees) =
        -- Takes children nodes and evaluates them using the negamax with
        -- alpha-beta pruning optimisation, then take the column of the best
        -- option.
        snd . iterateChildren (-pseudoInf) pseudoInf (-pseudoInf, 0)
            $ childTrees
    where -- Leaf node. Return the evaluation of this node using the
          -- 'winning rows' method.
          evalGameTree' (Node (GN board _ pl _) []) _ _ =
            evaluate pl 1 (-1) board
          -- Internal node. Evaluate children and get the best rating.
          evalGameTree' (Node _ childTrees) alpha beta =
            fst . iterateChildren alpha beta (-pseudoInf, 0) $ childTrees
          -- Evaluate children nodes using depth-first search.
          -- No remaining children. Return the data of the best child
          -- (rating and column).
          iterateChildren _     _    best [] = best
          -- There are children remaining. Calculate new alpha value, new best
          -- child and check pruning case.
          iterateChildren alpha beta (bestRating, bestCol) (t:ts) =
            let childValue = -(evalGameTree' t (-beta) (-alpha))
                newBestRat = max bestRating childValue
                newAlpha   = max alpha childValue
                newBestCol = if newBestRat > bestRating 
                                then gnColumn . rootLabel $ t
                                else bestCol
                newBest    = (newBestRat, newBestCol)
            in  if newAlpha >= beta
                    then newBest
                    else iterateChildren newAlpha beta newBest ts
          pseudoInf = 1000000

-- |The exported strategy.
negamaxStrategy :: Int -> GameStrategy
negamaxStrategy maxDepth = do
    player <- view gsCurrentPlayer
    curBoard <- view gsBoard

    --Return the result of evaluating the game tree.
    return . evalGameTree $ genGameTree maxDepth (GN curBoard 0 player 0)
