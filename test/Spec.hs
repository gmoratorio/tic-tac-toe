import Test.Hspec
import Lib (Row(..), Column(..), Entry(..), boardIsFull, boardHasWinner, updateCell)
import qualified Data.Map as M


main :: IO ()
main = hspec $ do
  describe "Given a full board with no winner" $ do
    it "Should detect that boardIsFull" $ do
      boardIsFull fullBoard `shouldBe` True
  
  describe "Given a board that is not yet full" $ do
    it "Should detect that the board is not full" $ do
      boardIsFull column3XWinnerBoard `shouldBe`  False
      boardIsFull row2OWinnerBoard `shouldBe`     False
      boardIsFull ltrDiagXWinnerBoard `shouldBe`  False

  describe "Given a board with a column winner X" $ do
    it "Should detect that X wins" $ do
      boardHasWinner column3XWinnerBoard X `shouldBe` True
    it "Should detect that O does not win" $ do
      boardHasWinner column3XWinnerBoard O `shouldBe` False

  describe "Given a board with a row winner O" $ do
    it "Should detect that O wins" $ do
      boardHasWinner row2OWinnerBoard O `shouldBe` True
    it "Should detect that X does not win" $ do
      boardHasWinner row2OWinnerBoard X  `shouldBe` False

  describe "Given a board with left-to-right diag winner X" $ do
    it "Should detect that X wins" $ do
      boardHasWinner ltrDiagXWinnerBoard X `shouldBe` True
    it "Should detect that O does not win" $ do
      boardHasWinner ltrDiagXWinnerBoard O `shouldBe` False

  describe "Given a board that is mid-game with no winners" $ do
    it "Should detect that there is currently no winner" $ do
      boardHasWinner midGameBoard X `shouldBe` False
      boardHasWinner midGameBoard O `shouldBe` False
    it "Should detect that the board is not full" $ do
      boardIsFull midGameBoard `shouldBe` False
    it "Should correctly place new entry, when a selected cell is open" $ do
      let (updated, newBoard) = updateCell midGameBoard (Row3, Col3) X
      updated `shouldBe` True
    it "Should not update the board if a selected cell is full" $ do
      let (updated, newBoard) = updateCell midGameBoard (Row1, Col1) X
      updated `shouldBe` False
    it "Should correctly detect that there is a winner if new cell entry would win the game" $ do
      let (updated, newBoard) = updateCell midGameBoard (Row3, Col3) X
      boardHasWinner newBoard X `shouldBe` True


fullBoard = M.fromList [((Row1, Col1), Just X), ((Row1, Col2), Just O), ((Row1, Col3), Just X), ((Row2, Col1), Just O), ((Row2, Col2), Just X), ((Row2, Col3), Just O) ,((Row3, Col1), Just O), ((Row3, Col2), Just X), ((Row3, Col3), Just O)]
column3XWinnerBoard = M.fromList [((Row1, Col1), Nothing), ((Row1, Col2), Nothing), ((Row1, Col3), Just X), ((Row2, Col1), Nothing), ((Row2, Col2), Nothing), ((Row2, Col3), Just X) ,((Row3, Col1), Nothing), ((Row3, Col2), Nothing), ((Row3, Col3), Just X)]
row2OWinnerBoard = M.fromList [((Row1, Col1), Nothing), ((Row1, Col2), Nothing), ((Row1, Col3), Nothing), ((Row2, Col1), Just O), ((Row2, Col2), Just O), ((Row2, Col3), Just O) ,((Row3, Col1), Nothing), ((Row3, Col2), Nothing), ((Row3, Col3), Just X)]
ltrDiagXWinnerBoard = M.fromList [((Row1, Col1), Just X), ((Row1, Col2), Nothing), ((Row1, Col3), Nothing), ((Row2, Col1), Just O), ((Row2, Col2), Just X), ((Row2, Col3), Just O) ,((Row3, Col1), Nothing), ((Row3, Col2), Nothing), ((Row3, Col3), Just X)]
midGameBoard = M.fromList [((Row1, Col1), Just X), ((Row1, Col2), Nothing), ((Row1, Col3), Nothing), ((Row2, Col1), Just O), ((Row2, Col2), Just X), ((Row2, Col3), Just O) ,((Row3, Col1), Nothing), ((Row3, Col2), Nothing), ((Row3, Col3), Nothing)]