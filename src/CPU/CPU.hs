
module CPU.CPU where

import System.IO
import System.Random
import CPU.Utility as Util

windowHeight = 32
windowWidth  = 64

{- Represents a centralised state for the CPU of the CHIP-8.
   The CHIP-8's state is the inner workings of the computer. It holds all the necessary data
   that it needs to have in order to execute any instructions and to interpret any data.

   INVARIANT: 0 <= pc < 2^12
              0 <= sp < 16^1
              0 <= i  < 2^16
              sound_timer >= 0
              delay_timer >= 0
              Every number in v is 8-bit.
              Every number in memory is 8-bit.
              Every number in stack is 16-bit.
-}
-- The design for the CPU is largely based on the version described in the wikipedia
-- article regarding the CHIP-8.
-- https://en.wikipedia.org/wiki/CHIP-8#Virtual_machine_description
data CPU = Cpu { v :: [Int]             -- 16 V Registers with 8-bit registrars. Index 0, 1, 2 ... E, F.
               , i :: Int               -- 16 bit register for memory address.
               , sound_timer :: Int     -- Timer for playing sounds.
               , delay_timer :: Int     -- Used to delay certain operations.
               , pc :: Int              -- Pointer to memory for current opcode.
               , memory :: [Int]        -- Place to store program data (instructions). 4096 bytes.
               , stack :: [Int]         -- Stack. List of 16 16-bit values.
               , sp :: Int              -- Pointer to current place in the stack.
               , vram :: [[Int]]        -- Memory containing what pixels are to be drawed on the screen.
               , keyboard :: [Bool]     -- List with bools representing if a certain key has been pressed.
               , rgen :: StdGen         -- Random number generator.
               , isRunning :: Bool      -- Flag if the emulator is running.
               } deriving (Show)

-- Returns a fresh state of the CPU where all of its values has been
-- set to their initial values.
initCPU :: [Int] -> StdGen -> CPU
initCPU rom randomgen = Cpu { v = replicate 16 0
                            , i = 0x200
                            , sound_timer = 0
                            , delay_timer = 0
                            , pc = 0x200
                            , memory = initMemory rom
                            , stack = replicate 16 0
                            , sp = 0
                            , vram = defaultVRAM
                            , keyboard = replicate 16 False
                            , rgen = randomgen
                            , isRunning = False
                            }

-- Changes isRunning field of a CPU to True.
startCPU :: CPU -> CPU
startCPU cpu = cpu {isRunning = True}

-- Returns a blank VRAM state.
defaultVRAM :: [[Int]]
defaultVRAM = replicate windowHeight (replicate windowWidth 0)

{- initMemory rom
   Loads the fontset and a rom onto a CPUs memory.

   RETURNS: fontset ++ (zeros up to adress 0x200) ++ program ++ (zeros to fill out rest of memory)
   EXAMPLES: initMemory ('rom' containing 3 characters) = fontset ++ (zeros to index 512) ++ [13,10,35] ++ (zeros to index 4096)
-}
initMemory :: [Int] -> [Int]
initMemory rom = fontset ++ replicate (0x200 - length fontset) 0 ++ padRom rom

{- padRom rom
   Pads rom with empty data to fill up memory.

   RETURNS: a list of length 3854 consisting of rom ++ (replicate (3854 - length rom) 0)
   EXAMPLES: padRom [1,2,3,4] = [1,2,3,4] ++ (replicate 3850 0)
-}
padRom :: [Int] -> [Int]
padRom rom
  | memLeft < 0 = error "Program too large"
  | null rom = error "File error"
  | otherwise = rom ++ replicate memLeft 0
    where memLeft = 0xE00 - length rom

{- setKey key bool keyboard
   Sets a key in a keyboard to True or False.

   PRE: length keyboard = 16
   RETURNS: if key is a valid input, it updates the index of that key in keyboard to bool,
            otherwise the unaltered keyboard is returned.
   EXAMPLES: setKey 'a' True  (replicate 16 False) == (list where index 7 is True)
             setKey 's' False (replicate 16 True)  == (list where index 8 is False and the rest is True)
             setKey 'g' True  (replicate 16 False) == (replicate 16 False)
-}
setKey :: Char -> Bool -> [Bool] -> [Bool]
setKey '1' b keys = Util.replace 0x1 b keys
setKey '2' b keys = Util.replace 0x2 b keys
setKey '3' b keys = Util.replace 0x3 b keys
setKey '4' b keys = Util.replace 0xC b keys
setKey 'q' b keys = Util.replace 0x4 b keys
setKey 'w' b keys = Util.replace 0x5 b keys
setKey 'e' b keys = Util.replace 0x6 b keys
setKey 'r' b keys = Util.replace 0xD b keys
setKey 'a' b keys = Util.replace 0x7 b keys
setKey 's' b keys = Util.replace 0x8 b keys
setKey 'd' b keys = Util.replace 0x9 b keys
setKey 'f' b keys = Util.replace 0xE b keys
setKey 'z' b keys = Util.replace 0xA b keys
setKey 'x' b keys = Util.replace 0x0 b keys
setKey 'c' b keys = Util.replace 0xB b keys
setKey 'v' b keys = Util.replace 0xF b keys
setKey _ _ keys = keys

-- Fontset for drawing characters to the screen.
fontset :: [Int]
fontset = [ 0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
          , 0x20, 0x60, 0x20, 0x20, 0x70 -- 1
          , 0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
          , 0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
          , 0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
          , 0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
          , 0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
          , 0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
          , 0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
          , 0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
          , 0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
          , 0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
          , 0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
          , 0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
          , 0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
          , 0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
          ]
