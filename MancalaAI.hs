module MancalaAI(aiNextMove) where

import MancalaBoard
import Data.List
import Data.Maybe

type Move = Int

-- Decides what move to make for the current player of the specified MancalaBoard.

aiNextMove :: MancalaBoard -> Move
aiNextMove mancala = lookahead mancala 4 -- slowly increase the depth

evalPosition :: Int -> MancalaBoard -> Int
evalPosition 0 mancala = (mult $ numCaptured mancala p) + (numTotal mancala) - (mult $ numCaptured mancala next) + canGoAgain where
             p = getCurPlayer mancala
             next
                  | p == PlayerA = PlayerB
                  | p == PlayerB = PlayerA
             canGoAgain = (length $ goAgainList p moveList) - (length $ goAgainList next moveList)
             moveList = map (move mancala) (allowedMoves mancala)
             numTotal m = sum $ map (getBoardData m !!) (allowedMoves m)
             mult a
                  | a < 10 = (10 - a) * a
                  | otherwise = 2 * a

evalPosition depth mancala =
             if (gameOver mancala)
                then
                  evalPosition 0 mancala
                else
                  multiplier * (evalPosition (depth - 1) newMancala) where
             newMancala = move mancala newMove
             newMove = lookahead mancala (depth - 1)
             multiplier
                | getCurPlayer mancala == getCurPlayer newMancala = 1
                | otherwise = -1

lookahead :: MancalaBoard -> Int -> Move
-- lookahead uses evalPosition at the same depth to determine the next
-- move to make
lookahead mancala depth = (allowedMoves mancala) !! foundMove where
          foundMove =
                    if (goAgain (current) moveList)
                       then
                        found $ elemIndex (getBoardData (last (goAgainList current moveList))) moveListBoard
                       else
                        last $ elemIndices topMove moveResults
          topMove = last $ sort $ moveResults
          am = allowedMoves mancala
          am2
              | length am > 1 = delete topNum am
              | otherwise = am
          moveResults = map (evalPosition depth) moveList
--        moveList = map (move mancala) (allowedMoves mancala)
          moveList = map (move mancala) am2
          moveListBoard = map getBoardData moveList
          bd = getBoardData mancala
          moveNums = map (bd !!) am
          topNum = am !! (found $ elemIndex (head $ sort moveNums) moveNums )
          found :: Maybe Int -> Int
          found m
                | isJust m = fromJust m
                | otherwise = 0


          current = getCurPlayer mancala

goAgain :: Player -> [MancalaBoard] -> Bool
goAgain p movelist = or (map ((== p) . getCurPlayer) movelist)

goAgainList :: Player -> [MancalaBoard] -> [MancalaBoard]
goAgainList p movelist = filter ((==p) . getCurPlayer) movelist
