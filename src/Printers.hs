module Printers
    ( printStandardBoard
    , convertBoardToString
    ) where

import Data.List
import Lib

header = "123"

convertEntriesToString :: [Maybe Entry] -> String
convertEntriesToString = foldl' (\acc maybeEntry -> acc ++ getStringFromEntry maybeEntry) ""

convertFormatRowToEntryRow :: Board -> [(Row, Column)] -> [Maybe Entry]
convertFormatRowToEntryRow board  = map (getEntry board)

convertBoardToString :: Board -> BoardFormat -> String 
convertBoardToString board formatRows = 
    let entryRows = map (convertFormatRowToEntryRow board) formatRows
        stringRows = map convertEntriesToString entryRows
        headerRow = "\n  " ++ header ++ "\n"
        rowsWithColHeaders = zipWith (\char string -> char : ' ' : string) header stringRows
    in  headerRow ++ unlines rowsWithColHeaders

printStandardBoard :: Board -> IO ()
printStandardBoard board = putStrLn $ convertBoardToString board ticTacToeBoardFormat