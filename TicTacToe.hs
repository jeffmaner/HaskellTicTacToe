module TicTacToe
  (Board,
   Icon (..),
   Play (..),
   block,
   canPlace,
   hasWinner,
   isFull,
   place,
   play,
   threats) where

data Icon = Empty | X | O deriving (Eq,Show)

type Board = [[Icon]]

data Play = InPlay | WinnerX | WinnerO | Draw deriving (Eq,Show)

hasWinner :: Board -> (Bool, Icon)
hasWinner [[a,b,c],_,_]             | same3Icons a b c = (True,a)
hasWinner [_,[a,b,c],_]             | same3Icons a b c = (True,a)
hasWinner [_,_,[a,b,c]]             | same3Icons a b c = (True,a)
hasWinner [[a,_,_],[b,_,_],[c,_,_]] | same3Icons a b c = (True,a)
hasWinner [[_,a,_],[_,b,_],[_,c,_]] | same3Icons a b c = (True,a)
hasWinner [[_,_,a],[_,_,b],[_,_,c]] | same3Icons a b c = (True,a)
hasWinner [[a,_,_],[_,b,_],[_,_,c]] | same3Icons a b c = (True,a)
hasWinner [[_,_,a],[_,b,_],[c,_,_]] | same3Icons a b c = (True,a)
hasWinner otherwise = (False,Empty)

isFull :: Board -> Bool
isFull rows =
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
place b i (x,y)
  | canPlace b i (x,y) = place' [] [] 0 0
  | otherwise          = error "An icon already exists there."
  where place' ba ra r c
          | r==x && c==y = ba ++ [(reverse $ i:ra) ++ restOfRow b r c] ++ restOfRows b r
          | c==2       = place' ((reverse (b!!r!!c:ra)):ba) [] (r+1) 0
          | otherwise = place' ba (b!!r!!c:ra) r (c+1)
        restOfRow b r c = drop (c+1) $ head $ restOfRows b r
        restOfRows b r =  drop (r+1) b

canPlace :: Board -> Icon -> (Int,Int) -> Bool
canPlace b i (x,y) = isEmpty 0 0
  where isEmpty r c
          | r==x && c==y = b!!r!!c==Empty
          | r<x       = isEmpty (r+1) c
          | c<y       = isEmpty r (c+1)
          | otherwise = error "Invalid square."

threats :: Board -> [[(Int,Int)]]
threats [[a,b,Empty],[_,_,_],[_,_,_]] | same2Icons a b = [[(0,0),(0,1)]]
threats [[a,Empty,b],[_,_,_],[_,_,_]] | same2Icons a b = [[(0,0),(0,2)]]
threats [[Empty,a,b],[_,_,_],[_,_,_]] | same2Icons a b = [[(0,1),(0,2)]]
threats [[_,_,_],[a,b,Empty],[_,_,_]] | same2Icons a b = [[(1,0),(1,1)]]
threats [[_,_,_],[a,Empty,b],[_,_,_]] | same2Icons a b = [[(1,0),(1,2)]]
threats [[_,_,_],[Empty,a,b],[_,_,_]] | same2Icons a b = [[(1,1),(1,2)]]
threats [[_,_,_],[_,_,_],[a,b,Empty]] | same2Icons a b = [[(2,0),(2,1)]]
threats [[_,_,_],[_,_,_],[a,Empty,b]] | same2Icons a b = [[(2,0),(2,2)]]
threats [[_,_,_],[_,_,_],[Empty,a,b]] | same2Icons a b = [[(2,1),(2,2)]]
threats [[a,_,_],[b,_,_],[Empty,_,_]] | same2Icons a b = [[(0,0),(1,0)]]
threats [[a,_,_],[Empty,_,_],[b,_,_]] | same2Icons a b = [[(0,0),(2,0)]]
threats [[Empty,_,_],[a,_,_],[b,_,_]] | same2Icons a b = [[(1,0),(2,0)]]
threats [[_,a,_],[_,b,_],[_,Empty,_]] | same2Icons a b = [[(0,1),(1,1)]]
threats [[_,a,_],[_,Empty,_],[_,b,_]] | same2Icons a b = [[(0,1),(2,1)]]
threats [[_,Empty,_],[_,a,_],[_,b,_]] | same2Icons a b = [[(1,1),(2,1)]]
threats [[_,_,a],[_,_,b],[_,_,Empty]] | same2Icons a b = [[(0,2),(1,2)]]
threats [[_,_,a],[_,_,Empty],[_,_,b]] | same2Icons a b = [[(0,2),(2,2)]]
threats [[_,_,Empty],[_,_,a],[_,_,b]] | same2Icons a b = [[(1,2),(2,2)]]
threats [[a,_,_],[_,b,_],[_,_,Empty]] | same2Icons a b = [[(0,0),(1,1)]]
threats [[a,_,_],[_,Empty,_],[_,_,b]] | same2Icons a b = [[(0,0),(2,2)]]
threats [[Empty,_,_],[_,a,_],[_,_,b]] | same2Icons a b = [[(1,1),(2,2)]]
threats [[_,_,a],[_,b,_],[Empty,_,_]] | same2Icons a b = [[(0,2),(1,1)]]
threats [[_,_,a],[_,Empty,_],[b,_,_]] | same2Icons a b = [[(0,2),(2,0)]]
threats [[_,_,Empty],[_,a,_],[b,_,_]] | same2Icons a b = [[(1,1),(2,0)]]
threats otherwise = []

same3Icons :: Icon -> Icon -> Icon -> Bool
same3Icons a b c = a==b && b==c && same2Icons a b

same2Icons :: Icon -> Icon -> Bool
same2Icons a b = a==b && isXO a && isXO b

isXO :: Icon -> Bool
isXO i = i `elem` [X,O]

block :: Board -> (Int,Int) -> Board
block b X (x,y) = place b O (x,y)
block b O (x,y) = place b X (x,y)

-- TODO: Redesign threat recognition tests.
