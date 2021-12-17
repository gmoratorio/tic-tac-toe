module Lib
    ( blankBoard
    , getCoord
    , getEntry
    , getCell
    , printCellEntry
    , playTurn
    , convertRowToString
    , convertRowToStringWithHeader
    , convertBoardToString
    , convertBoardToStringWithHeaders
    , printBoard
    , printBoardWithHeaders
    , runGame
    ) where

import Data.List
import System.Console.ANSI

data PlayerEntry = X | Y
type Entry = Char
type Coord = (Int, Int)
type Cell = (Entry, Coord)
type Row = [Cell]
type Board = [Row]

getCell :: Board -> Coord -> Cell
getCell board (x,y) = board !! x !! y

getCells :: Board -> [Coord] -> Row
getCells board coords = map (\coord -> getCell board coord) coords

getCoord :: Cell -> Coord
getCoord (_, coord) = coord

getEntry :: Cell -> Entry
getEntry (entry, _) = entry

cellIsEmpty :: Cell -> Bool
cellIsEmpty (entry, _) = entry == '_'

isSameCell :: Cell -> Cell -> Bool
isSameCell (_, (x,y)) (_, (x',y')) = (x == x') && (y == y')

translateBoardEntryToDisplay :: Int -> Int
translateBoardEntryToDisplay x = x + 1

translateUserEntryToBoard :: Int -> Int
translateUserEntryToBoard x = x - 1

getNextEntry :: Entry -> Entry
getNextEntry entry = if entry == 'X' then 'O' else 'X'

rowHasWinner :: Row -> Entry -> Bool
rowHasWinner cells entry = all (\cell -> entry == (getEntry cell)) cells

diagHasWinner :: Board -> Entry -> Bool
diagHasWinner board entry =
    let ltrCoords = [(0,0),(1,1),(2,2)]
        rtlCoords = [(2,0),(1,1),(0,2)]
        ltrDiagRow = getCells board ltrCoords
        rtlDiagRow = getCells board rtlCoords
        ltrHasWin = rowHasWinner ltrDiagRow entry
        rtlHasWin = rowHasWinner rtlDiagRow entry
    in  ltrHasWin || rtlHasWin



boardHasWinner :: Board -> Entry -> Bool
boardHasWinner board entry =
  let rowWin = any (\row -> rowHasWinner row entry) board
      transposedBoard = transpose board
      columnWin = any (\row -> rowHasWinner row entry) transposedBoard
      diagWin = diagHasWinner board entry
  in rowWin || columnWin || diagWin


-- basic translations
convertRowToString :: Row -> String
convertRowToString cells = map (\(entry, coords) -> entry) cells

convertBoardToString :: Board -> String
convertBoardToString board  =
      let rows = map convertRowToString board
      in unlines rows

printBoard :: Board -> IO ()
printBoard board = putStr $ convertBoardToString board


-- with headers
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


playTurn :: Board -> Entry -> IO ()
playTurn board entry = do
  if entry == 'X' then
      setSGR [SetColor Foreground Dull Cyan]
  else setSGR [SetColor Foreground Dull Magenta]
  putStrLn $ "Player " ++ show entry ++ " is up!\n"
  putStrLn "Current Board State: "
  printBoardWithHeaders board
  putStrLn "\nEnter Row: "
  rowResponse <- getLine
  putStrLn "Enter Column: "
  columnResponse <- getLine
  let x = read rowResponse :: Int
      y = read columnResponse :: Int
      nextEntry = getNextEntry entry
      adjustedX = translateUserEntryToBoard x
      adjustedY = translateUserEntryToBoard y
      newBoard = updateCellInBoard board (entry,(adjustedX,adjustedY))
  if boardHasWinner newBoard entry then do
        clearScreen
        setSGR [SetColor Foreground Dull Green]
        printBoardWithHeaders newBoard
        putStrLn $ "CONGRATULATIONS TO PLAYER " ++ show entry ++ " YOU ARE THE WINNER!!"
  else do
        clearScreen
        playTurn newBoard nextEntry

isCellOpen :: Board -> Coord -> Bool
isCellOpen board (x,y) = cellIsEmpty $ getCell board (x,y)

updateCell :: Entry -> Cell -> Cell
updateCell newEntry (entry, coord) = if cellIsEmpty (entry, coord) then (newEntry, coord) else (entry, coord)

updateRow :: Row -> Cell -> Row
updateRow cells newCell = map (\cell -> if (isSameCell newCell cell) && (cellIsEmpty cell) then newCell else cell) cells

updateCellInBoard :: Board -> Cell -> Board
updateCellInBoard board newCell = map (\row -> updateRow row newCell) board

runGame :: IO ()
runGame = do
    clearScreen
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn "\n~~Welcome to Tic Tac Toe!~~\n"
    playTurn blankBoard 'X'


blankBoard =  [ [('_', (0,0)),('_', (0,1)),('_', (0,2))]
              , [('_', (1,0)),('_', (1,1)),('_', (1,2))]
              , [('_', (2,0)),('_', (2,1)),('_', (2,2))]
              ]
