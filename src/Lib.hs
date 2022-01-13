module Lib
    ( getNextEntry
    , getEntry
    , boardHasWinner
    , boardIsFull
    , createBlankBoard
    , ticTacToeBoardFormat
    , getStringFromEntry
    , getEntryFromString
    , getRowFromChar
    , getColFromChar
    , updateCell
    , Board
    , BoardFormat
    , Row (..)
    , Column (..)
    , Cell
    , Coord
    , Entry (..)
    ) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

data Entry = X | O deriving (Eq, Show)
data Row = Row1 | Row2 | Row3 deriving (Eq, Ord, Show)
data Column = Col1 | Col2 | Col3 deriving (Eq, Ord, Show)

type Coord = (Row, Column)
type Cell = (Coord, Maybe Entry) -- is this needed?
type Board = M.Map Coord (Maybe Entry)
type BoardFormat = [[Coord]]

getEntry :: Board -> Coord -> Maybe Entry
getEntry board coord = M.findWithDefault Nothing coord board

getStringFromEntry :: Maybe Entry -> String
getStringFromEntry    Nothing    = "_"
getStringFromEntry    (Just a)   = show a

getEntryFromString :: String -> Maybe Entry
getEntryFromString "X"  = Just X
getEntryFromString "O"  = Just O
getEntryFromString  _   = Nothing

getRowFromChar :: Char -> Maybe Row
getRowFromChar '1'  = Just Row1
getRowFromChar '2'  = Just Row2
getRowFromChar '3'  = Just Row3
getRowFromChar  _   = Nothing

getColFromChar :: Char -> Maybe Column
getColFromChar '1'  = Just Col1
getColFromChar '2'  = Just Col2
getColFromChar '3'  = Just Col3
getColFromChar  _   = Nothing

updateCell :: Board -> Coord -> Entry -> (Bool, Board)
updateCell board coord entry =
    let existingEntry = getEntry board coord
        entryIsEmpty = isNothing existingEntry
        newBoard = if entryIsEmpty then  M.adjust (const (Just entry)) coord board else board
    in (entryIsEmpty, newBoard)

getNextEntry :: Entry -> Entry
getNextEntry entry = if entry == X then O else X

getCoordsForRow :: Board -> Row -> [Coord]
getCoordsForRow board row = M.keys $ M.filterWithKey (\(r, _) _ -> r == row) board

getCoordsForCol :: Board -> Column -> [Coord]
getCoordsForCol board col = M.keys $ M.filterWithKey (\(_, c) _ -> c == col) board

getEntries :: Board -> [Coord] -> [Maybe Entry]
getEntries board coords = M.elems $ M.filterWithKey (\boardCoord _ -> boardCoord `elem` coords) board

hasWinner :: Eq a => a -> [a] -> Bool 
hasWinner x = all (== x)

coordsHaveWinner :: Board -> [Coord] -> Entry -> Bool
coordsHaveWinner board coords entry = hasWinner (Just entry) $ getEntries board coords

rowHasWinner :: Board -> Row -> Entry -> Bool
rowHasWinner board row  = coordsHaveWinner board (getCoordsForRow board row)

colHasWinner :: Board -> Column -> Entry -> Bool 
colHasWinner board column = coordsHaveWinner board (getCoordsForCol board column)

diagHasWinner :: Board -> Entry -> Bool
diagHasWinner board entry =
    let ltrCoords = [(Row1, Col1),(Row2, Col2),(Row3, Col3)]
        rtlCoords = [(Row3, Col1),(Row2, Col2),(Row1, Col3)]
        ltrHasWin = coordsHaveWinner board ltrCoords entry
        rtlHasWin = coordsHaveWinner board rtlCoords entry
    in  ltrHasWin || rtlHasWin


boardHasWinner :: Board -> Entry -> Bool
boardHasWinner board entry =
  let rowWin = any (\row -> rowHasWinner board row entry) [Row1, Row2, Row3]
      columnWin = any (\col -> colHasWinner board col entry) [Col1, Col2, Col3]
      diagWin = diagHasWinner board entry
  in rowWin || columnWin || diagWin

boardIsFull :: Board -> Bool
boardIsFull board = all (/= Nothing) $  M.elems board

createBlankBoard :: Board
createBlankBoard = 
    let flatList = concat ticTacToeBoardFormat
        flatListWithNothing = map (\coord -> (coord, Nothing)) flatList
        board = M.fromList flatListWithNothing
    in board

ticTacToeBoardFormat :: BoardFormat
ticTacToeBoardFormat =      [[(Row1, Col1),(Row1, Col2),(Row1, Col3)]
                            ,[(Row2, Col1),(Row2, Col2),(Row2, Col3)]
                            ,[(Row3, Col1),(Row3, Col2),(Row3, Col3)]
                            ]
