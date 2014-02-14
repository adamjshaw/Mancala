import MancalaBoard
import MancalaAI

main :: IO ()
main = do
     putStrLn "Welcome to Mancala!"
     putStrLn "Type a number to move from that number's pit"
     endBoard <- makeMove initial
     putStrLn (show endBoard)
     let wlist = winners endBoard
     if (length wlist > 1)
        then putStrLn ("It's a tie!")
        else putStrLn ("The winner is: " ++ show wlist)
     putStrLn ("Would you like to play again? (y/n)")
     answer <- getLine
     if (answer == "y")
        then main
        else putStrLn ("Thanks for playing!")

makeMove :: MancalaBoard -> IO MancalaBoard
makeMove m = do
         putStrLn ("Current Board: \n" ++ show m)
         moveNum <- chooseMove m
         if (isAllowedMove m moveNum)
            then do
                 let newBoard = move m moveNum
                 if (not (gameOver newBoard))
                    then do
                         makeMove newBoard
                    else do
                         return newBoard
            else do
                 putStrLn ("Move not allowed \nAllowedMoves: "++ show (allowedMoves m))
                 makeMove m




chooseMove :: MancalaBoard -> IO Int
chooseMove m = do
           if (getCurPlayer m == PlayerB)
              then do
                   s <- getLine
                   let moveNum = read s :: Int
                   return moveNum
              else return $ aiNextMove m
