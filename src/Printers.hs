module Printers
    ( printCellEntry
    , printBoardWithHeaders
    , translateUserEntryToBoard
    ) where

import Lib

translateBoardEntryToDisplay :: Int -> Int
translateBoardEntryToDisplay x = x + 1

translateUserEntryToBoard :: Int -> Int
translateUserEntryToBoard x = x - 1

convertRowToStringWithHeader :: Row -> String
convertRowToStringWithHeader cells =
      let (_, (rowNumber, _)) = cells !! 0
          rowString = map (\(entry, coords) -> entry) cells
          adjustedRowNumber = translateBoardEntryToDisplay rowNumber
      in show adjustedRowNumber ++ " " ++  rowString

convertBoardToStringWithHeaders :: Board -> String
convertBoardToStringWithHeaders board  =
      let rows = map convertRowToStringWithHeader board
          headerRow = "  123"
          allRows = headerRow : rows
      in  unlines allRows

printBoardWithHeaders :: Board -> IO ()
printBoardWithHeaders board = putStr $ convertBoardToStringWithHeaders board


printCellEntry :: Board -> Coord -> IO ()
printCellEntry board (x,y) =
  let (entry, _) = getCell board (x,y)
      printout = "Entry at (" ++ (show (translateBoardEntryToDisplay x)) ++ "," ++ (show (translateBoardEntryToDisplay y)) ++ ") is " ++ show entry
  in putStrLn printout
