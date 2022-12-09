module Main (main) where

-- Import the necessary modules

import Data.List
import System.Environment
import System.IO
import qualified System.IO.Strict as S

-- Define the main function
main = do
  -- Get the command line arguments
  args <- getArgs

  -- Check if the user provided any arguments
  if length args == 0
    then do
      putStrLn "Usage: todo [command] [task]"
    else do
      -- Parse the command and task from the arguments
      let command = head args
      let task = unwords $ tail args

      -- Check which command the user entered
      case command of
        "add" -> addTask task
        "remove" -> removeTask task
        "view" -> viewTasks
        _ -> putStrLn "Unknown command"

-- Define a function to add a new task to the TODO list
addTask :: String -> IO ()
addTask task = do
  -- Open the TODO list file in append mode
  handle <- openFile "todo.txt" AppendMode

  -- Append the new task to the file
  hPutStrLn handle (task ++ "\n")

  -- Close the file handle
  hClose handle

-- Define a function to remove a task from the TODO list
removeTask :: String -> IO ()
removeTask task = do
  -- Open the TODO list file in read mode
  readHandle <- openFile "todo.txt" ReadMode

  -- Read the contents of the file
  contents <- S.hGetContents readHandle

  -- Remove the task from the list of tasks
  let tasks = delete task $ lines contents

  -- Close the file handle
  hClose readHandle

  -- Open the TODO list file in write mode
  writeHandle <- openFile "todo.txt" WriteMode

  -- Overwrite the file with the updated list of tasks
  hPutStr writeHandle $ unlines tasks

  -- Close the file handle
  hClose writeHandle

-- Define a function to view the tasks in the TODO list
viewTasks :: IO ()
viewTasks = do
  -- Open the TODO list file in read mode
  handle <- openFile "todo.txt" ReadMode

  -- Read the contents of the file
  contents <- hGetContents handle

  -- Print each task on its own line
  putStr . unlines . filter (/= "") . lines $ contents
