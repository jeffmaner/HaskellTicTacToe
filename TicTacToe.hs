module TicTacToe where

data Icon = Empty | X | O deriving (Eq,Show)

data Board = Board [[Icon]] deriving (Eq,Show)

data Play = InPlay | WinnerX | WinnerO | Draw deriving (Eq,Show)

hasWinner :: Board -> (Bool, Icon)
hasWinner (Board [[X,X,X],_,_]) = (True,X)
hasWinner (Board [_,[X,X,X],_]) = (True,X)
hasWinner (Board [_,_,[X,X,X]]) = (True,X)
hasWinner (Board [[X,_,_],[X,_,_],[X,_,_]]) = (True,X)
hasWinner (Board [[_,X,_],[_,X,_],[_,X,_]]) = (True,X)
hasWinner (Board [[_,_,X],[_,_,X],[_,_,X]]) = (True,X)
hasWinner (Board [[X,_,_],[_,X,_],[_,_,X]]) = (True,X)
hasWinner (Board [[_,_,X],[_,X,_],[X,_,_]]) = (True,X)
hasWinner (Board [[O,O,O],_,_]) = (True,O)
hasWinner (Board [_,[O,O,O],_]) = (True,O)
hasWinner (Board [_,_,[O,O,O]]) = (True,O)
hasWinner (Board [[O,_,_],[O,_,_],[O,_,_]]) = (True,O)
hasWinner (Board [[_,O,_],[_,O,_],[_,O,_]]) = (True,O)
hasWinner (Board [[_,_,O],[_,_,O],[_,_,O]]) = (True,O)
hasWinner (Board [[O,_,_],[_,O,_],[_,_,O]]) = (True,O)
hasWinner (Board [[_,_,O],[_,O,_],[O,_,_]]) = (True,O)
hasWinner otherwise = (False,Empty)

isFull :: Board -> Bool
isFull (Board rows) =
  let xs = count X
      os = count O
   in xs+os==9
  where count i = length . filter (==i) . foldl1 (++) $ rows

play :: Board -> Play
play b = case (isFull b, hasWinner b) of
           (_, (True,X)) -> WinnerX
           (_, (True,O)) -> WinnerO
           (True, _)     -> Draw
           otherwise     -> InPlay

place :: Board -> Icon -> (Int,Int) -> Board
place b i (x,y) = undefined
