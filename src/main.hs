
module Main where

import CPU.CPU as CPU
import CPU.LoadRom as LoadRom
import CPU.Emulate as Emulate (emulateCycle)
import CLI.Cli as CLI (getRomInfo)
import Render.Renderer as Render
import System.Random
import Data.Time.Clock.POSIX
import Graphics.Gloss.Interface.Environment (getScreenSize)

-- Starts emulator
main :: IO ()
main = do
  (path, fps) <- CLI.getRomInfo True -- False -> GHCi, True -> Cabal
  rom         <- LoadRom.readRom path
  size        <- getScreenSize
  rndSeed     <- fmap round getPOSIXTime
  let displaySettings  = Render.Settings size fps
  let cpu = CPU.initCPU rom (mkStdGen rndSeed)
  Render.startRenderer displaySettings cpu onInput onUpdate

-- Called on input
{- onInput key isDown cpu
   Registers and unregisters accepted key inputs in the current gamestate or changes nothing.

   RETURNS: cpu where key in keyboard changes or returns cpu
   EXAMPLES: onInput 'a' True (default cpu)  == (cpu where keyboard index 7 is set as True)
             onInput 'g' True (default cpu)  == (default cpu)
             onInput 'g' False (default cpu) == (default cpu)
-}
onInput :: Char -> Bool -> CPU -> CPU
onInput key isDown cpu = cpu {keyboard = CPU.setKey key isDown (keyboard cpu)}

-- Called every frame before onRenderer
-- Calls Emulate.emulateCycle if currently running
onUpdate :: Float -> CPU -> CPU
onUpdate _ cpu
  | isRunning cpu = Emulate.emulateCycle cpu
  | otherwise     = cpu
