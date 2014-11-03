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
      hasWinner [[X,X,X],[],[]] `shouldBe` (True, X)

    it "Recognizes three Xs in the middle row as a winner." $
      hasWinner [[],[X,X,X],[]] `shouldBe` (True, X)

    it "Recognizes three Xs in the bottom row as a winner." $
      hasWinner [[],[],[X,X,X]] `shouldBe` (True, X)

    it "Recognizes three Xs in the first column as a winner." $
      hasWinner [[X,Empty,Empty],[X,Empty,Empty],[X,Empty,Empty]] `shouldBe` (True, X)

    it "Recognizes three Xs in the middle column as a winner." $
      hasWinner [[Empty,X,Empty],[Empty,X,Empty],[Empty,X,Empty]] `shouldBe` (True, X)

    it "Recognizes three Xs in the last column as a winner." $
      hasWinner [[Empty,Empty,X],[Empty,Empty,X],[Empty,Empty,X]] `shouldBe` (True, X)

    it "Recognizes three Xs in the \\ diagonal as a winner." $
      hasWinner [[X,Empty,Empty],[Empty,X,Empty],[Empty,Empty,X]] `shouldBe` (True, X)

    it "Recognizes three Xs in the / diagonal as a winner." $
      hasWinner [[Empty,Empty,X],[Empty,X,Empty],[X,Empty,Empty]] `shouldBe` (True, X)

    it "Recognizes three Os in the top row as a winner." $
      hasWinner [[O,O,O],[],[]] `shouldBe` (True, O)

    it "Recognizes three Os in the middle row as a winner." $
      hasWinner [[],[O,O,O],[]] `shouldBe` (True, O)

    it "Recognizes three Os in the bottom row as a winner." $
      hasWinner [[],[],[O,O,O]] `shouldBe` (True, O)

    it "Recognizes three Os in the first column as a winner." $
      hasWinner [[O,Empty,Empty],[O,Empty,Empty],[O,Empty,Empty]] `shouldBe` (True, O)

    it "Recognizes three Os in the middle column as a winner." $
      hasWinner [[Empty,O,Empty],[Empty,O,Empty],[Empty,O,Empty]] `shouldBe` (True, O)

    it "Recognizes three Os in the last column as a winner." $
      hasWinner [[Empty,Empty,O],[Empty,Empty,O],[Empty,Empty,O]] `shouldBe` (True, O)

    it "Recognizes three Os in the \\ diagonal as a winner." $
      hasWinner [[O,Empty,Empty],[Empty,O,Empty],[Empty,Empty,O]] `shouldBe` (True, O)

    it "Recognizes three Os in the / diagonal as a winner." $
      hasWinner [[Empty,Empty,O],[Empty,O,Empty],[O,Empty,Empty]] `shouldBe` (True, O)

    it "Recognizes a full board." $
      isFull [[X,O,X],[O,X,O],[O,X,O]] `shouldBe` True

    it "Recognizes a board in-play." $
      play [[X,Empty,O],[O,X,Empty],[Empty,Empty,Empty]] `shouldBe` InPlay

    it "Determines it cannot place a piece onto the board." $
      canPlace [[],[Empty,O,Empty],[]] (1,1) `shouldBe` False

    it "Determines it can place a piece onto the board." $
      canPlace [[Empty,Empty,Empty],[Empty,O,Empty],[]] (0,0) `shouldBe` True

    it "Places a piece onto the NW corner of the board." $
      place [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] X (0,0) `shouldBe` [[X,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]

    it "Places a piece onto the center of the board." $
      place [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] X (1,1) `shouldBe` [[Empty,Empty,Empty],[Empty,X,Empty],[Empty,Empty,Empty]]

    it "Places a piece onto the NE corner of the board." $
      place [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] X (0,2) `shouldBe` [[Empty,Empty,X],[Empty,Empty,Empty],[Empty,Empty,Empty]]

    it "Recognizes the threat of two icons in a row." $
      threats [[X,X,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] `shouldBe` Just (0,2)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,X,X],[Empty,Empty,Empty],[Empty,Empty,Empty]] `shouldBe` Just (0,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[X,X,Empty],[Empty,Empty,Empty]] `shouldBe` Just (1,2)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,X,X],[Empty,Empty,Empty]] `shouldBe` Just (1,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,Empty,Empty],[X,X,Empty]] `shouldBe` Just (2,2)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,X,X]] `shouldBe` Just (2,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[X,Empty,Empty],[X,Empty,Empty],[Empty,Empty,Empty]] `shouldBe` Just (2,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[X,Empty,Empty],[X,Empty,Empty]] `shouldBe` Just (0,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,X,Empty],[Empty,X,Empty],[Empty,Empty,Empty]] `shouldBe` Just (2,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,X,Empty],[Empty,X,Empty]] `shouldBe` Just (0,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,X],[Empty,Empty,X],[Empty,Empty,Empty]] `shouldBe` Just (2,2)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,Empty,X],[Empty,Empty,X]] `shouldBe` Just (0,2)

    it "Recognizes the threat of two icons in a row." $
      threats [[X,Empty,X],[Empty,Empty,Empty],[Empty,Empty,Empty]] `shouldBe` Just (0,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[X,Empty,X],[Empty,Empty,Empty]] `shouldBe` Just (1,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,Empty,Empty],[X,Empty,X]] `shouldBe` Just (2,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[X,Empty,Empty],[Empty,Empty,Empty],[X,Empty,Empty]] `shouldBe` Just (1,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,X,Empty],[Empty,Empty,Empty],[Empty,X,Empty]] `shouldBe` Just (1,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,X],[Empty,Empty,Empty],[Empty,Empty,X]] `shouldBe` Just (1,2)

    it "Recognizes the threat of two icons in a row." $
      threats [[X,Empty,Empty],[Empty,X,Empty],[Empty,Empty,Empty]] `shouldBe` Just (2,2)

    it "Recognizes the threat of two icons in a row." $
      threats [[X,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,X]] `shouldBe` Just (1,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,X,Empty],[Empty,Empty,X]] `shouldBe` Just (0,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,X],[Empty,X,Empty],[Empty,Empty,Empty]] `shouldBe` Just (2,0)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,X],[Empty,Empty,Empty],[X,Empty,Empty]] `shouldBe` Just (1,1)

    it "Recognizes the threat of two icons in a row." $
      threats [[Empty,Empty,Empty],[Empty,X,Empty],[X,Empty,Empty]] `shouldBe` Just (0,2)

    it "Blocks threats." $ do
      let b = [[Empty,Empty,Empty],[Empty,X,Empty],[X,Empty,Empty]]
          t = case threats b of
                Just x  -> x
                Nothing -> error "Expected threat."
      block b X t `shouldBe` [[Empty,Empty,O],[Empty,X,Empty],[X,Empty,Empty]]

    it "Prefers center over corners." $
      preference [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] `shouldBe` (1,1)

    it "Prefers corners over middle edges." $
      preference [[Empty,Empty,Empty],[Empty,X,Empty],[Empty,Empty,Empty]] `shouldBe` (0,1)
