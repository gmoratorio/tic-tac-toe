module Main where

import Lib
import Printers
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
    playTurn blankBoard 'X'


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



blankBoard =  [ [('_', (0,0)),('_', (0,1)),('_', (0,2))]
              , [('_', (1,0)),('_', (1,1)),('_', (1,2))]
              , [('_', (2,0)),('_', (2,1)),('_', (2,2))]
              ]
