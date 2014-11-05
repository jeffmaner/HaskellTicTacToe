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
  putStrLn "Okay!"
  putStrLn "Please imagine the upper-left corner of the board to be square 0,"
  putStrLn "the top middle, square 1, etc."

  let f b = playRound b userIcon computerIcon
  let g b = if inPlay b
            then do b' <- f b
                    g b'
            else return ()

  g [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]

  putStrLn "Good game!"

parsePreference :: String -> Maybe Icon
parsePreference s =
  case toLower $ head s of
    'x'  -> Just X
    'o'  -> Just O
    otherwise -> Nothing

playRound :: Board -> Icon -> Icon -> IO Board
playRound b u c = play' b playUser u >>=
  \b'->if inPlay b'
      then play' b' playComputer c
      else return b'
  where play' b p i = do
          b' <- p b i
          draw b'
          case play b' of
            WinnerX -> do declareWinner b' X
            WinnerO -> do declareWinner b' O
            Draw    -> do declareDraw b'
            InPlay  -> return b'

inPlay :: Board -> Bool
inPlay = (InPlay ==) . play

playUser, playComputer :: Board -> Icon -> IO Board
playUser b i = do
  c <- putStr "Please indicate your move (q to quit). " >> getLine
  let c' = numToCoord (read c :: Int)
  if isValidMove $ head c
  then if canPlace b c'
       then return $ place b i c'
       else error "There's already an icon there."
  else case toLower $ head c of
         'q'       -> error "Quitting."
         otherwise -> error "Ack! Bad input."

playComputer b i = do
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
