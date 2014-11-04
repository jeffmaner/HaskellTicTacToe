module Game where

import Data.Char (toLower)
--import System.IO (hFlush, stdout)
import System.IO (BufferMode (..), hSetBuffering, stdout)
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
  hSetBuffering stdout NoBuffering

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

  let board = [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]

  board0 <- playUser board userIcon
  board1 <- playComputer board0 computerIcon
  board2 <- playUser board1 userIcon
  board3 <- playComputer board2 computerIcon
  board4 <- playUser board3 userIcon
  board5 <- playComputer board4 computerIcon
  board6 <- playUser board5 userIcon
  board7 <- playComputer board6 computerIcon
  board8 <- playUser board7 userIcon
  putStrLn "Good game!"

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
    WinnerX -> declareWinner b X
    WinnerO -> declareWinner b O
    Draw    -> declareDraw b
    InPlay  -> play' ()
  where play' () = do
          c <- putStr "Please indicate your move. " >> getChar
          let c' = numToCoord c
          if isValidMove c
          then if canPlace b c'
               then return $ place b i c'
               else error "There's already an icon there."
          else error "Ack! Bad input."

playComputer b i =
  case play b of
    WinnerX -> declareWinner b X
    WinnerO -> declareWinner b O
    Draw    -> declareDraw b
    InPlay  -> play' ()
  where play' () = do
          let p = case threats b of
                    Just x  -> x
                    Nothing -> preference b
          putStrLn $ "I play square " ++ coordToNum p ++ "."
          return $ place b i p

declareWinner :: Board -> Icon -> IO Board
declareWinner b i = do
  putStrLn $ (show i) ++ " wins!"
  return b

declareDraw :: Board -> IO Board
declareDraw b = do
  putStrLn "It's a draw."
  return b

-- TODO: Change these two functions to an association list.
numToCoord :: Char -> (Int,Int)
numToCoord '0' = (0,0)
numToCoord '1' = (0,1)
numToCoord '2' = (0,2)
numToCoord '3' = (1,0)
numToCoord '4' = (1,1)
numToCoord '5' = (1,2)
numToCoord '6' = (2,0)
numToCoord '7' = (2,1)
numToCoord '8' = (2,2)

coordToNum :: (Int,Int) -> String
coordToNum (0,0) = "0"
coordToNum (0,1) = "1"
coordToNum (0,2) = "2"
coordToNum (1,0) = "3"
coordToNum (1,1) = "4"
coordToNum (1,2) = "5"
coordToNum (2,0) = "6"
coordToNum (2,1) = "7"
coordToNum (2,2) = "8"
