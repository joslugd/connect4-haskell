{-
 - Strategy (AI) implemented using the negamax algorithm.
 - -}

module Strategy.Negamax
(
    negamaxStrategy
) where

import Data.List (maximumBy, zipWith4)
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
-- As we will be using the Negamax algorithm to evaluate the tree, this
-- laziness doesn't make a difference (because it needs to visit the whole
-- tree) but it will help when we implement alpha-beta pruning.
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
                -- Get lists of possible boards and used columns.
                (possiblePlays, columns) = unzip $ mapMaybe genNode
                                                   [0..boardCols-1]
                newDepth = depth + 1
                -- Generate childs of current node.
                childNodes = if newDepth > maxDepth
                                then []
                                else zipWith4 GN possiblePlays
                                                 columns
                                                 (repeat $ nextPlayer player)
                                                 (repeat newDepth)
            in  (node, childNodes)

-- |Eval the game tree using the negamax algorithm, returning the best
-- column selection for the current player.
evalGameTree :: Tree GameNode -> Int
evalGameTree (Node _ childTrees) =
        -- Takes every children, evaluates them and gets the column selection
        -- for the one with the best rating.
        gnColumn . snd . maximumBy (comparing fst) .
        map (\subt -> (-(evalGameTree' subt), rootLabel subt)) $ childTrees
    where -- Leaf node. Return the evaluation of this node using the
          -- 'winning rows' method.
          evalGameTree' (Node (GN board _ pl _) []) =
            evaluate pl 1 (-1) board
          -- Internal node. Evaluate children and return the best rating.
          evalGameTree' (Node _ childTrees) =
            maximum . map (negate . evalGameTree') $ childTrees

-- |The exported strategy.
negamaxStrategy :: Int -> GameStrategy
negamaxStrategy maxDepth = do
    player <- view gsCurrentPlayer
    curBoard <- view gsBoard

    --Return the result of evaluating the game tree.
    return . evalGameTree $ genGameTree maxDepth (GN curBoard 0 player 0)
