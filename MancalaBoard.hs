module MancalaBoard (MancalaBoard, Player, initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners) where

import List -- for List.elemIndex
import Maybe -- for List.elemIndex

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board

{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
getCurPlayer (MancalaBoardImpl _ p) = p


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
getBoardData (MancalaBoardImpl l _) = l

{- return the side of the board for a specified player, including the store at
 - the end -}
playerSide :: MancalaBoard -> Player -> [Int]
playerSide (MancalaBoardImpl l _) p = take (boardSize + 1) (drop (indexForFirstPit p) l)



{- return the number of captured pieces in specified player's store -}
numCaptured :: MancalaBoard -> Player -> Int
numCaptured (MancalaBoardImpl l _) p = l !! (indexForPlayerStore p)

{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
allowedMoves :: MancalaBoard -> [Int]
allowedMoves (MancalaBoardImpl l p) = [a | a <- indicesForPlayerSide p, l !! a  > 0]


{- check that a move is valid for the current player -}
isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove board move = move `elem` (allowedMoves board)

{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}
move :: MancalaBoard -> Int -> MancalaBoard
move (MancalaBoardImpl l p) moveInd =
     if (isAllowedMove board moveInd)
        then do
             --add one to where the stone ends
             let l2 = replaceN moveEnd (moveEndVal + 1) l
             --take a stone away from the pit
             let l3 = replaceN moveInd (moveVal - 1) l2
             --move again with the new list and one less stone
             let movedBoard = move (MancalaBoardImpl l3 p) moveInd
             MancalaBoardImpl (getBoardData (movedBoard)) pFinal
        else
             MancalaBoardImpl l p
                                                                where
        --moveEnd makes sure the stone will skip the other player's store

        moveEnd
                | x == (indexForPlayerStore (nextPlayer p)) = y
                | otherwise = x
                where
                        x = (moveInd + moveVal) `mod` (length l)
                        y = (x + 1) `mod` (length l)
        moveEndVal = l !! moveEnd
        --this is for unboxing
        board = (MancalaBoardImpl l p)
        moveVal = l !! moveInd
        --at the end, it checks whether the current player should change
        pFinal = maybeTurn p
        maybeTurn p
                  | moveEnd == indexForPlayerStore p = p
                  | otherwise = nextPlayer p


--helper function to replace the nth term in a list
replaceN n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceN (n-1) newVal xs

{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
gameOver board = null $ allowedMoves board

{- winner returns a list of players who have the top score: there will only be
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
winners (MancalaBoardImpl l p) =
        [ps | ps <- allPlayers, l !! (indexForPlayerStore ps) >= l !! indexForPlayerStore (nextPlayer ps)]


---- show
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) =
            "   PlayerA pits: " ++ (show x) ++ " \n   PlayerB pits: " ++ (show y) ++ " \n   Current Player: " ++ (show player)
                     where
            (x,y) = splitAt ((indexForPlayerStore PlayerA) + 1) boardData
