module Main where

import Lib
import Printers
import Data.Maybe
import Data.Char
import System.IO
import System.Console.ANSI

main :: IO ()
main = do
  runGame

runGame :: IO ()
runGame = do
    clearScreen
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn "\n~~Welcome to Tic Tac Toe!~~\n"
    playTurn createBlankBoard X

getRowInput :: Board -> Entry -> IO (Char)
getRowInput board entry = do
      putStrLn "\nEnter Row: "
      rowInput <- getChar
      let row = getRowFromChar rowInput
      if isNothing row then do
            outOfBoundsWarning rowInput
            showCurrentBoard board entry
            getRowInput board entry
      else do
            return rowInput

getColInput :: Board -> Entry -> Char -> IO (Char)
getColInput board entry rowInput = do
      putStrLn "\nEnter Row: "
      putStrLn $ show rowInput
      putStrLn "\n\nEnter Column: "
      colInput <- getChar
      let col = getColFromChar colInput
      if isNothing col then do
            outOfBoundsWarning colInput
            showCurrentBoard board entry
            getColInput board entry rowInput
      else do
            return colInput

getCoords :: Board -> Entry -> IO (Coord)
getCoords board entry = do
      rowInput <- getRowInput board entry
      let row = getRowFromChar rowInput
      clearScreen
      showCurrentBoard board entry
      colInput <- getColInput board entry rowInput
      let col = getColFromChar colInput
      return (fromJust row, fromJust col)

outOfBoundsWarning :: Char -> IO ()
outOfBoundsWarning input = do
                        clearScreen
                        putStrLn "\nSorry, you must select a number from 1 to 3 for both Row and Column."
                        putStrLn $ "\nYou entered " ++ (show input)
                        putStrLn "\nLet's try again!"

spaceTakenRestart :: Board -> Coord -> Entry -> IO ()
spaceTakenRestart board coord entry = do
                        clearScreen
                        putStrLn "\nSorry, the space you selected is already taken!"
                        putStrLn $ "\nYou entered " ++ (show coord)
                        putStrLn "\nLet's try again!\n\n"
                        playTurn board entry

offerNewGame :: IO ()
offerNewGame = do
      putStrLn "\n\nWant to play again? [y/n]\n"
      resp <- getChar
      if resp == 'y' then do
            runGame
      else do
            putStrLn "\nNo worries, come back whenever you want to play again!"

showCurrentBoard :: Board -> Entry -> IO ()
showCurrentBoard board entry = do
      putStrLn "Current Board State: "
      printStandardBoard board
      putStrLn $ "Player " ++ show entry ++ " is up!\n"


playTurn :: Board -> Entry -> IO ()
playTurn board entry = do
      if entry == X then
            setSGR [SetColor Foreground Dull Cyan]
      else  setSGR [SetColor Foreground Dull Magenta]
      showCurrentBoard board entry
      coord <- getCoords board entry
      let (updated, newBoard) = updateCell board coord entry
      if not updated then do
            spaceTakenRestart board coord entry
      else do
            if boardHasWinner newBoard entry then do
                  clearScreen
                  setSGR [SetColor Foreground Dull Green]
                  printStandardBoard newBoard
                  putStrLn $ "CONGRATULATIONS TO PLAYER " ++ show entry ++ " YOU ARE THE WINNER!!"
                  offerNewGame
            else do
                  if boardIsFull newBoard then do
                        clearScreen
                        setSGR [SetColor Foreground Dull Red]
                        printStandardBoard newBoard
                        putStrLn "Well shoot! No winner this time :("
                        offerNewGame
                  else do
                        let nextEntry = getNextEntry entry
                        clearScreen
                        playTurn newBoard nextEntry
      


