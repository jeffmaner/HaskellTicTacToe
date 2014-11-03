module Game where

import TicTacToe

-- I'll use [these][1] as the requirements:
--
-- Tic-Tac-Toe is a board-game. It has two players. Each player has an icon
-- depicting a nought ( O ) or a cross ( X ). The board is divided into 3X3
-- squares and each square can be occupied by one icon. If three icons of same
-- type appear in a row either vertically or horizantally or diagonally, the
-- user of that icon will be declared winner by the system. If all the squares
-- of the board are filled and no user is the winner then the game is declared
-- as draw by the system. If a player quits the game then his opponent will be
-- declared winner by the system. Upon starting the game, the system should
-- allow the players to make moves alternatively or to quit.
--
-- [1]: http://vu.bits-pilani.ac.in/Ooad/Lesson10/topic9.htm

main :: IO ()
main = do
  let board = [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]

  putStr "Welcome to Tic-Tac-Toe. Would you like to be Xs or Os? "
  userPreference <- getLine
  let preference = parsePreference userPreference
  let (userIcon, computerIcon) = case preference of
                                   Just X    -> (X,O)
                                   Just O    -> (O,X)
                                   otherwise -> error "Unrecognized icon specified."
  putStrLn "Okay! I don't have fancy graphics yet, so you'll have to keep track of the game board yourself."
  putStrLn "Please imagine the upper-left corner of the board to be square 0,"
  putStrLn "the top middle, square 1, etc."

  let board0 = playUser board userIcon
  let board1 = playComputer board0 computerIcon
  let board2 = playUser board1 userIcon
  let board3 = playComputer board2 computerIcon
  let board4 = playUser board3 userIcon
  let board5 = playComputer board4 computerIcon
  let board6 = playUser board5 userIcon
  let board7 = playComputer board6 computerIcon
  let board8 = playUser board7 userIcon

parsePreference :: String -> Maybe Icon
parsePreference s =
  case toLower $ head s of
    'x'  -> Just X
    'o'  -> Just O
    otherwise -> Nothing

isValidMove :: Char -> Bool
isValidMove = (`elem` ['0'..'8'])

playUser, playComputer :: Board -> Icon -> IO Board

playUser b i =
  case play b of
    WinnerX -> do putStrLn "X wins!"; return b
    WinnerO -> do putStrLn "O wins!"; return b
    Draw    -> do putStrLn "It's a draw."; return b
    InPlay  -> do putStr "Please indicate your move. "
                 -- Parse error on c.
                 c <- getChar
                 let c' = numToCoord c
                 if isValidMove c' && canPlace b c'
                 then place b i c'
                 else error "Ack! Bad input."

playComputer b i =
  case play b of
    WinnerX -> do putStrLn "X wins!"; return b
    WinnerO -> do putStrLn "O wins!"; return b
    Draw    -> do putStrLn "It's a draw."; return b
    InPlay  -> do let ts = threats b
                 let p = case ts of
                           Just x  -> x
                           Nothing -> preference b
                 putStrLn "I play square " ++ coordToNum p ++ "."
                 place b i p

-- TODO: Change these two functions to an association list.
numToCoord :: Char -> (Int,Int)
numToCoord
  | '0' -> (0,0)
  | '1' -> (0,1)
  | '2' -> (0,2)
  | '3' -> (1,0)
  | '4' -> (1,1)
  | '5' -> (1,2)
  | '6' -> (2,0)
  | '7' -> (2,1)
  | '8' -> (2,2)

coordToNum :: (Int,Int) -> String
coordToNum
  | (0,0) -> "0"
  | (0,1) -> "1"
  | (0,2) -> "2"
  | (1,0) -> "3"
  | (1,1) -> "4"
  | (1,2) -> "5"
  | (2,0) -> "6"
  | (2,1) -> "7"
  | (2,2) -> "8"
