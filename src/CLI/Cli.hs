
module CLI.Cli where

import Data.Char
import System.Directory
import System.Environment
import System.IO

{- getRomInfo usingCabal
   Asks the user to choose a ROM found in the specified path and fetches the relative path to that rom.

   PRE: there are files in "roms" folder
   RETURNS: the relative path to a rom
   SIDE EFFECTS: lists all files found in path, 
                 exception thrown if "roms" folder does not exist
   EXAMPLES: getRomInfo False (Input TANK) == "../roms/TANK"
             getRomInfo True (Input TANK)  == "roms/TANK"
-}
getRomInfo :: Bool -> IO (String, Int)
getRomInfo iscabal = do
  let pathStart = if iscabal then "ROMs/" else "../ROMs/"
  options <- listDirectory pathStart
  game    <- askForFile (reverse options)
  return (pathStart ++ game, getFPS game)

{- askForFile path options
   Repeatedly asks the user to chose one of the ROMs of the given options and 
   repeats the question if input is invalid. Returns the chosen ROMs relative 
   path and fps.

   PRE: options is not empty, path exists
   RETURNS: the relative path to a game and game specific fps
   SIDE EFFECTS: prints prompts in the terminal,
                 reads inputs from terminal.
   EXAMPLES: askForFile [TANK,PONG] (Input TANK) == "TANK"
-}
askForFile :: [String] -> IO String
askForFile options = do
  putStrLn $ "Available ROMs: " ++ buildString options
  putStr "Type in the ROM you would like to launch: "
  hFlush stdout
  inputStr <- getLine
  let str = map toUpper inputStr

  if str `elem` options
  then return str
  else do
    putStrLn "Invalid input. Try again!\n"
    askForFile options

{- getFPS name
   Gets the predefined fps for a ROM with the given name or returns the default value 100.

   RETURNS: returns the relative fps to name or the default value 100
   EXAMPLES: getFPS "PONG" == 60
             getFPS "X"    == 100
-}
getFPS :: String -> Int
getFPS key = findInList key list
  where
    list = [("15PUZZLE",320),("BLINKY",600),("CONNECT4",50),("HIDDEN", 80),("KALEID",600),("MAZE",300),("PONG",300),("TETRIS",120),("TICTAC",80),("VERS",120)]
    -- Matches a given key to a list value
    findInList :: String -> [(String, Int)] -> Int
    findInList str ((key,fps):xs)
      | str == key = fps
      | otherwise  = findInList str xs
    findInList _ _   = 100

{- buildString list
   Formats a list into a nice looking string.

   PRE: list is not empty
   RETURNS: a string with each element in list separated with ", "
   EXAMPLES: buildString ["hej"]      == "hej"
             buildString ["hej", "a"] == "hej, a"
-}
buildString :: [String] -> String
buildString [x]    = x
buildString (x:xs) = x ++ (',' : ' ' : buildString xs)
