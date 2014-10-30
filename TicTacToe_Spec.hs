module TicTacToe_Spec where

import Test.Hspec
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
main = hspec $ do
  describe "Tic-Tac-Toe" $ do
    it "Recognizes three Xs in the top row as a winner." $
      hasWinner (Board [[X,X,X],[],[]]) `shouldBe` (True, X)

    it "Recognizes three Xs in the middle row as a winner." $
      hasWinner (Board [[],[X,X,X],[]]) `shouldBe` (True, X)

    it "Recognizes three Xs in the bottom row as a winner." $
      hasWinner (Board [[],[],[X,X,X]]) `shouldBe` (True, X)

    it "Recognizes three Xs in the first column as a winner." $
      hasWinner (Board [[X,Empty,Empty],[X,Empty,Empty],[X,Empty,Empty]]) `shouldBe` (True, X)

    it "Recognizes three Xs in the middle column as a winner." $
      hasWinner (Board [[Empty,X,Empty],[Empty,X,Empty],[Empty,X,Empty]]) `shouldBe` (True, X)

    it "Recognizes three Xs in the last column as a winner." $
      hasWinner (Board [[Empty,Empty,X],[Empty,Empty,X],[Empty,Empty,X]]) `shouldBe` (True, X)

    it "Recognizes three Xs in the \\ diagonal as a winner." $
      hasWinner (Board [[X,Empty,Empty],[Empty,X,Empty],[Empty,Empty,X]]) `shouldBe` (True, X)

    it "Recognizes three Xs in the / diagonal as a winner." $
      hasWinner (Board [[Empty,Empty,X],[Empty,X,Empty],[X,Empty,Empty]]) `shouldBe` (True, X)

    it "Recognizes three Os in the top row as a winner." $
      hasWinner (Board [[O,O,O],[],[]]) `shouldBe` (True, O)

    it "Recognizes three Os in the middle row as a winner." $
      hasWinner (Board [[],[O,O,O],[]]) `shouldBe` (True, O)

    it "Recognizes three Os in the bottom row as a winner." $
      hasWinner (Board [[],[],[O,O,O]]) `shouldBe` (True, O)

    it "Recognizes three Os in the first column as a winner." $
      hasWinner (Board [[O,Empty,Empty],[O,Empty,Empty],[O,Empty,Empty]]) `shouldBe` (True, O)

    it "Recognizes three Os in the middle column as a winner." $
      hasWinner (Board [[Empty,O,Empty],[Empty,O,Empty],[Empty,O,Empty]]) `shouldBe` (True, O)

    it "Recognizes three Os in the last column as a winner." $
      hasWinner (Board [[Empty,Empty,O],[Empty,Empty,O],[Empty,Empty,O]]) `shouldBe` (True, O)

    it "Recognizes three Os in the \\ diagonal as a winner." $
      hasWinner (Board [[O,Empty,Empty],[Empty,O,Empty],[Empty,Empty,O]]) `shouldBe` (True, O)

    it "Recognizes three Os in the / diagonal as a winner." $
      hasWinner (Board [[Empty,Empty,O],[Empty,O,Empty],[O,Empty,Empty]]) `shouldBe` (True, O)

    it "Recognizes a full board." $
      isFull (Board [[X,O,X],[O,X,O],[O,X,O]]) `shouldBe` True

    it "Recognizes a board in-play." $
      play (Board [[X,Empty,O],[O,X,Empty],[Empty,Empty,Empty]]) `shouldBe` InPlay

    it "Places a piece onto the board." $
       place (Board [[],[],[]]) X (1,1) `shouldBe` Board [[Empty,Empty,Empty],[Empty,X,Empty],[Empty,Empty,Empty]]
