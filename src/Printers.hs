module Printers
    ( printStandardBoard
    , convertBoardToString
    ) where

import Data.List
import Data.List.Split
import Lib
import qualified Data.Map as M
import Control.Applicative

header = "123"

convertEntriesToString :: [Maybe Entry] -> String
convertEntriesToString = foldl' (\acc maybeEntry -> acc ++ getStringFromEntry maybeEntry) ""

convertFormatRowToEntryRow :: Board -> [(Row, Column)] -> [Maybe Entry]
convertFormatRowToEntryRow  = map . flip M.lookup 

convertBoardToString :: Board -> [Coord] -> String 
convertBoardToString board coords  = 
    let entryRows = convertFormatRowToEntryRow board <$> chunksOf 3 coords
        stringRows = convertEntriesToString <$> entryRows
        headerRow = "\n  " <> header <> "\n"
        rowsWithColHeaders = zipWith (\char string -> char : ' ' : string) header stringRows
    in  headerRow <> unlines rowsWithColHeaders

printStandardBoard :: Board -> IO ()
printStandardBoard board = putStrLn $ convertBoardToString board $ liftA2 (,) [minBound .. maxBound] [minBound .. maxBound]