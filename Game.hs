module Game where

import Data.Char (toLower)
import Data.List (elemIndex)
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
  draw board0
  board1 <- playComputer board0 computerIcon
  draw board1
  board2 <- playUser board1 userIcon
  draw board2
  board3 <- playComputer board2 computerIcon
  draw board3
  board4 <- playUser board3 userIcon
  draw board4
  board5 <- playComputer board4 computerIcon
  draw board5
  board6 <- playUser board5 userIcon
  draw board6
  board7 <- playComputer board6 computerIcon
  draw board7
  board8 <- playUser board7 userIcon
  draw board8
  putStrLn "Good game!"

parsePreference :: String -> Maybe Icon
parsePreference s =
  case toLower $ head s of
    'x'  -> Just X
    'o'  -> Just O
    otherwise -> Nothing

playUser, playComputer :: Board -> Icon -> IO Board

playUser b i = do
  case play b of
    WinnerX -> declareWinner b X
    WinnerO -> declareWinner b O
    Draw    -> declareDraw b
    InPlay  -> play' b i
  where play' b i = do
          c <- putStr "Please indicate your move. " >> getLine >>= \x->return x
          let c' = numToCoord (read c :: Int)
          if isValidMove $ head c
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
          putStrLn $ "I play square " ++ (show $ coordToNum p) ++ "."
          return $ place b i p

isValidMove :: Char -> Bool
isValidMove = (`elem` ['0'..'8'])

declareWinner :: Board -> Icon -> IO Board
declareWinner b i = do
  putStrLn $ (show i) ++ " wins!"
  return b

declareDraw :: Board -> IO Board
declareDraw b = do
  putStrLn "It's a draw."
  return b

coordinates = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]

numToCoord :: Int -> (Int,Int)
numToCoord = (coordinates!!)

coordToNum :: (Int,Int) -> Int
coordToNum c = case c `elemIndex` coordinates of
                 Just n  -> n
                 Nothing -> error "Invalid coordinate."

draw :: Board -> IO ()
draw [r1@[a,b,c],r2@[d,e,f],r3@[g,h,i]] = do
  drawRow r1
  drawRow r2
  drawRow r3
  where drawRow [x,y,z] = do
          drawIcon x
          putStr "|"
          drawIcon y
          putStr "|"
          drawIcon z
          putStrLn ""
        drawIcon i = case i of
                       Empty     -> do putStr " "
                       otherwise -> do putStr $ show i
