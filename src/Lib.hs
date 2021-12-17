module Lib
    ( getCell
    , getNextEntry
    , updateCellInBoard
    , boardHasWinner
    , Board
    , Row
    , Cell
    , Coord
    , Entry
    ) where

import Data.List

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

isCellOpen :: Board -> Coord -> Bool
isCellOpen board (x,y) = cellIsEmpty $ getCell board (x,y)

updateCell :: Entry -> Cell -> Cell
updateCell newEntry (entry, coord) = if cellIsEmpty (entry, coord) then (newEntry, coord) else (entry, coord)

updateRow :: Row -> Cell -> Row
updateRow cells newCell = map (\cell -> if (isSameCell newCell cell) && (cellIsEmpty cell) then newCell else cell) cells

updateCellInBoard :: Board -> Cell -> Board
updateCellInBoard board newCell = map (\row -> updateRow row newCell) board
