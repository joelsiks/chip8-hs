
module CPU.Emulate where

import Control.Lens
import CPU.CPU as CPU
import CPU.Utility as Util
import Data.Bits
import System.Random
import System.Exit

{- Represents an instruction for the CHIP-8.
   Opcode is a two-byte value that is stored in big-endian format.
   Each Integer in Opcode is a hexadecimal value between 0-F.
   The two first Ints in Opcode corresponds to the first byte of the instruction, 
   and the last two corresponds to the second byte. 

   INVARIANT: Each integer in Opcode is a hexadecimal number, i.e 0-9 or A-F.
-}
type Opcode = (Int, Int, Int, Int)

{- emulateCycle cpu
   Emulates one cycle of a CPU.

   RETURNS: a new cpu where an opcode has been fetched and executed, and sound_timer and delay_timer have been decremented.
   EXAMPLES: emulateCycle (cpu where opcode is to store val at idx) = (cpu where val is stored at idx)
             emulateCycle (cpu where opcode is to clear vram)       = (cpu with vram cleared)
             
-}
emulateCycle :: CPU -> CPU
emulateCycle cpu = decreseTimers $ executeOpcode cpu (fetchOpcode cpu)

{- decreseTimer cpu
   Decrements the delay timer and sound timer of a cpu by 1.

   RETURNS: a new cpu where sound_timer and/or delay_timer are decremented by 1 if either count is above 0
   EXAMPLES: decreseTimers (cpu where sound_timer = 6 & delay_timer = 4) = (cpu where sound_timer = 5 & delay_timer = 3)
             decreseTimers (cpu where sound_timer = 0 & delay_timer = 3) = (cpu where sound_timer = 0 & delay_timer = 2)
-}
decreseTimers :: CPU -> CPU
decreseTimers cpu = cpu {sound_timer = max 0 (sound_timer cpu - 1), delay_timer = max 0 (delay_timer cpu - 1)}

{- fetchOpcode cpu
   Fetches an opcode from the memory of a CPU.

   RETURNS: the four hexadecimal values that make up the two bytes that (pc cpu) and (pc cpu + 1) 
            points to in (memory cpu).
   EXAMPLES: fetchOpcode (cpu where (pc cpu and pc cpu + 1) points to [..., 0x12, 0x34, ..]) = (0x1, 0x2, 0x3, 0x4)
             fetchOpcode (cpu where (pc cpu and pc cpu + 1) points to [..., 0xA3, 0xD8, ..]) = (0xA, 0x3, 0xD, 0x8)
-}
fetchOpcode :: CPU -> Opcode
fetchOpcode cpu = (a1, a2, b1, b2)
  where
    (a1, a2) = Util.splitByte $ memory cpu !! pc cpu
    (b1, b2) = Util.splitByte $ memory cpu !! (pc cpu + 1)

{- executeOpcode cpu opcode
   Executes an opcode and alters the state of the CPU it was executed on.
   
   RETURNS: a new cpu where opcode has been executed and altered the state of cpu in some way.
   SIDE EFECTS: throws an exception if opcode is not valid
   EXAMPLES: executeOpcode (initialized CPU)   (0x0,0x0,0xE,0x0) = (cpu where vram cpu = vram filled with zeroes)
             executeOpcode (cpu where pc = 24) (0x2,0x1,0x2,0x3) = (cpu where sp increased by one, stack cpu !! (sp - 1) = 26 and pc = 0x123)
             executeOpcode (initialized CPU)   (0x6,0x3,0x4,0x9) = (cpu where v cpu !! 0x3 = 0x49)
-}
-- All opcode type signatures have been sourced from the wikipedia article regarding the CHIP-8.
-- https://en.wikipedia.org/wiki/CHIP-8#Opcode_table
executeOpcode :: CPU -> Opcode -> CPU
executeOpcode cpu opcode =
  case opcode of 
    -- 0x00E0: Clears the screen.
    (0x0, 0x0, 0xE, 0x0) ->
      incPC $ cpu {vram = defaultVRAM}
    -- 0x00EE: Returns from a subroutine.
    (0x0, 0x0, 0xE, 0xE) ->
      returnFromSubroutine cpu
    -- 0x1NNN: Jumps to address NNN.
    (0x1, _, _, _) ->
      jumpToAddress cpu $ opNNN opcode
    -- 0x2NNN: Calls subroutine at NNN.
    (0x2, _, _, _) ->
      callSubroutine cpu $ opNNN opcode
    -- 0x3XNN: Skips the next instruction if Vx == NN.
    (0x3, x, _, _) ->
      incPC $ skipInstructionIf cpu (v cpu !! x == opNN opcode)
    -- 0x4XNN: Skips the next instruction if Vx /= NN.
    (0x4, x, _, _) ->
      incPC $ skipInstructionIf cpu (v cpu !! x /= opNN opcode)
    -- 0x5XY0: Skips the next instruction if Vx == Vy
    (0x5, x, y, 0x0) ->
      incPC $ skipInstructionIf cpu (v cpu !! x == v cpu !! y)
    -- 0x6XNN: Sets Vx to NN.
    (0x6, x, _, _) ->
      incPC $ setRegister cpu x (opNN opcode)
    -- 0x7XNN: Adds NN to Vx.
    (0x7, x, _, _) ->
      incPC $ setRegister cpu x ((v cpu !! x + opNN opcode) `mod` 256)
    -- 0x8XY0: Set Vx to the value of Vy.
    (0x8, x, y, 0x0) ->
      incPC $ setRegister cpu x (v cpu !! y)
    -- 0x8XY1: Set Vx to (Vx OR Vy)
    (0x8, x, y, 0x1) ->
      incPC $ setRegister cpu x (v cpu !! x .|. v cpu !! y)
    -- 0x8XY2: Set Vx to (Vx AND Vy)
    (0x8, x, y, 0x2) ->
      incPC $ setRegister cpu x (v cpu !! x .&. v cpu !! y)
    -- 0x8XY3: Set Vx to (Vx XOR Vy)
    (0x8, x, y, 0x3) ->
      incPC $ setRegister cpu x (xor (v cpu !! x) (v cpu !! y))
    -- 0x8XY4: Adds Vy to Vx. Vf is set to 1 when there's a carry, and to 0 when there isn't. 
    (0x8, x, y, 0x4) ->
      let 
        addVal = v cpu !! x + v cpu !! y
        ucpu = setRegister cpu x (addVal .&. 0xFF)
      in 
        incPC $ setRegister ucpu 0xF (if addVal > 0xFF then 1 else 0)
    -- 0x8XY5: Vy is subtracted from Vx. Vf is set to 0 when there's a borrow, and 1 when there isn't. 
    (0x8, x, y, 0x5) ->
      let
        subVal = v cpu !! x - v cpu !! y
        ucpu = setRegister cpu 0xF (if subVal < 0 then 0 else 1)
      in 
        incPC $ setRegister ucpu x (subVal `mod` 256)
    -- 0x8XY6: Stores the least significant bit of Vx in Vf and then shifts Vx to the right by 1.
    (0x8, x, _, 0x6) ->
      incPC $ shiftRegRight cpu x
    -- 0x8XY7: Sets Vx to Vy minus Vx. Vf is set to 0 when there's a borrow, and 1 when there isn't. 
    (0x8, x, y, 0x7) ->
      let
        subVal = v cpu !! y - v cpu !! x
        ucpu = setRegister cpu 0xF (if subVal < 0 then 0 else 1)
      in 
        incPC $ setRegister ucpu x (subVal `mod` 256)
    -- 0x8XYE: Stores the most significant bit of VX in VF and then shifts VX to the left by 1.
    (0x8, x, _, 0xE) ->
      incPC $ shiftRegLeft cpu x
    -- 0x9XY0: Skips the next instruction if Vx /= Vy.
    (0x9, x, y, 0x0) ->
      incPC $ skipInstructionIf cpu ((v cpu !! x) /= (v cpu !! y))
    -- 0xANNN: Sets I to NNN.
    (0xA, _, _, _) ->
      incPC $ cpu {i = opNNN opcode}
    -- 0xBNNN: Jumps to the address V0 + NNN.
    (0xB, _, _, _) ->
      jumpToAddress cpu (head (v cpu) + opNNN opcode)
    -- 0xCXNN: Sets Vx to (a random value between 0-255) AND NN.
    (0xC, x, _, _) ->
      incPC $ generateRandomValue cpu x (opNN opcode)
    -- 0xDXYN: Draws a sprite at coordinate (Vx, Vy) that has a width of 8 pixels and a height of N pixels. 
    (0xD, x, y, n) ->
      incPC $ drawSprite cpu x y n
    -- 0xEX9E: Skips the next instruction if the key stored in Vx is pressed.
    (0xE, x, 0x9, 0xE) ->
      incPC $ skipInstructionIf cpu (keyboard cpu !! (v cpu !! x))
    -- 0xEXA1: Skips the next instruction if the key stored in Vx is not pressed.
    (0xE, x, 0xA, 0x1) ->
      incPC $ skipInstructionIf cpu (not (keyboard cpu !! (v cpu !! x)))
    -- 0xFX07: Sets Vx to the value of the delay_timer.
    (0xF, x, 0x0, 0x7) ->
      incPC $ setRegister cpu x (delay_timer cpu)
    -- 0xFX0A: A key press is awaited, and then stored in Vx. The emulator will only
    --         step to the next instruction if a key has been pressed, essentially
    --         halting the emulator.
    (0xF, x, 0x0, 0xA) ->
      checkIfInput cpu x
    -- 0xFX15: Sets the delay timer to Vx.
    (0xF, x, 0x1, 0x5) ->
      incPC $ cpu {delay_timer = v cpu !! x}
    -- 0xFX18: Sets the sound timer to Vx.
    (0xF, x, 0x1, 0x8) ->
      incPC $ cpu {sound_timer = v cpu !! x}
    -- 0xFX1E: Adds Vx to I. Vf is set to 1 when there is a range overflow (I+Vx > 0xFFF), and to 0 when there isn't.
    (0xF, x, 0x1, 0xE) ->
      incPC $ ucpu {i = (i cpu + v cpu !! x) `mod` floor 65535}
        where ucpu = setRegister cpu 0xF (if i cpu > x then 1 else 0)
    -- 0xFX29: Sets I to 5 * Vx.
    (0xF, x, 0x2, 0x9) ->
      incPC $ cpu {i = (v cpu !! x * 5) `mod` 65535}
    -- 0xFX33: Stores the binary coded decimal representation of Vx in memory.
    (0xF, x, 0x3, 0x3) ->
      incPC $ storeBCDRepresentation cpu x
    -- 0xFX55: Stores V0 to VX (including VX) in memory starting at address I. 
    --         The offset from I is increased by 1 for each value written, but I itself is left unmodified.
    (0xF, x, 0x5, 0x5) ->
      incPC $ storeRegisters cpu x
    -- 0xFX65: Fills V0 to VX (including VX) with values from memory starting at address I. 
    --         The offset from I is increased by 1 for each value written, but I itself is left unmodified.
    (0xF, x, 0x6, 0x5) ->
      incPC $ loadRegisters cpu x
    -- If an undefined opcode gets executed, the program exits with an error message.
    -- This is because an undefined opcode that cannot get executed could lead to undefined behaviour.
    _ -> error $ "Undefined opcode: " ++ show opcode ++ ". Exiting..."

-- Increments the program counter by 2 (one instruction).
incPC :: CPU -> CPU
incPC cpu = cpu {pc = pc cpu + 2}

-- Calculates the combined hex value of the first three 4-bit values in an Opcode.
opNNN :: Opcode -> Int
opNNN (_, x, y, z) = shift x 8 + shift y 4 + z

-- Calculates the combined hex value of the first two 4-bit values in an Opcode.
opNN :: Opcode -> Int
opNN (_, _, x, y) = shift x 4 + y

-- Jumps to a specific address by setting the program counter to that address.
jumpToAddress :: CPU -> Int -> CPU
jumpToAddress cpu addr = cpu {pc = addr}

-- Skips the next instruction if pred is True, otherwise return the same state.
skipInstructionIf :: CPU -> Bool -> CPU
skipInstructionIf cpu pred | pred      = incPC cpu
                           | otherwise = cpu

-- Calls a subroutine by inserting the current position onto the stack and jumping to
-- a specific address.
callSubroutine :: CPU -> Int -> CPU
callSubroutine cpu = jumpToAddress ucpu
  where ucpu = insertToStack cpu (pc (incPC cpu))

-- Returns from a subroutine by decrementing the stack pointer by 1 and jumping to the stored location
-- in (stack cpu !! (sp cpu - 1)).
returnFromSubroutine :: CPU -> CPU
returnFromSubroutine cpu = jumpToAddress ucpu (stack ucpu !! sp ucpu)
  where ucpu = cpu {sp = sp cpu - 1}

-- Inserts a given value onto the stack where the stack pointer points to,
-- and increments the stack pointer by 1.
insertToStack :: CPU -> Int -> CPU
insertToStack cpu val = ucpu {sp = sp cpu + 1}
  where ucpu = cpu {stack = Util.replace (sp cpu) val (stack cpu)}

-- Sets the register at idx to val.
setRegister :: CPU -> Int -> Int -> CPU
setRegister cpu idx val = cpu {v = Util.replace idx val (v cpu)}

-- Sets the memory position at pos to val.
setMemory :: CPU -> Int -> Int -> CPU
setMemory cpu pos val = cpu {memory = Util.replace pos val (memory cpu)}

{- shiftRegLeft cpu idx
   Shifts the value of a register to the left by 1 bit.

   PRE: 0 <= idx < 16
   RETURNS: cpu where (v cpu !! idx) = shiftL (v cpu !! idx) 1 and (v cpu !! 0xF) has been set to
            the MSB of (v cpu !! idx)
   EXAMPLES: shiftRegLeft (cpu where (v cpu !! 0) = 10)    0 = (cpu where (v cpu !! 0) = 20) and (v cpu !! 0xF) = 0
             shiftRegLeft (cpu where (v cpu !! 0) = 0xFF)  0 = (cpu where (v cpu !! 0) = 254) and (v cpu !! 0xF) = 1
-}
shiftRegLeft :: CPU -> Int -> CPU
shiftRegLeft cpu idx = setRegister ucpu idx $ shiftL (v cpu !! idx) 1 `mod` 256
  where ucpu = setRegister cpu 0xF (shiftR (v cpu !! idx) 7)

{- shiftRegRight cpu idx
   Shifts the value of a register to the right by 1 bit.

   PRE: idx is a single hexadecimal number.
   RETURNS: cpu where (v cpu !! idx) = shiftR (v cpu !! idx) 1 and (v cpu !! 0xF) has been set to
            the LSB of (v cpu !! idx)
   EXAMPLES: shiftRegRight (cpu where (v cpu !! 0) = 10) = (cpu where (v cpu !! 0) = 5) and (v cpu !! 0xF) = 0
             shiftRegRight (cpu where (v cpu !! 0) = 11) = (cpu where (v cpu !! 0) = 5) and (v cpu !! 0xF) = 1
-}
shiftRegRight :: CPU -> Int -> CPU
shiftRegRight cpu idx = setRegister ucpu idx $ shiftR (v cpu !! idx) 1
  where ucpu = setRegister cpu 0xF ((v cpu !! idx) .&. 1)

{- storeBCDRepresentation cpu idx
   Stores the binary-coded decimal representation of a number to memory.

   PRE: idx is a single hexadecimal number.
   RETURNS: cpu where the number in (v cpu !! idx) has been offloaded to the memory starting at position (i cpu).
   EXAMPLES: storeBCDRepresentation (cpu where v cpu !! 0x0 = 231) 0x0 = (cpu where part of memory contains [..., 2, 3, 1, ...])
             storeBCDRepresentation (cpu where v cpu !! 0x0 = 194) 0x0 = (cpu where part of memory contains [..., 1, 9, 4, ...])
-}
storeBCDRepresentation :: CPU -> Int -> CPU
storeBCDRepresentation cpu idx = setMemory ucpu2 (i cpu) (v cpu !! idx `div` 100)
  where
    ucpu2 = setMemory ucpu1 (i cpu + 1) ((v cpu !! idx `mod` 100) `div` 10)
      where
        ucpu1 = setMemory cpu (i cpu + 2) (v cpu !! idx `mod` 10)

-- Copies all the values from (v cpu) register to the memory starting at position (i cpu).
storeRegisters :: CPU -> Int -> CPU
storeRegisters cpu idx | idx == 0  = ucpu
                       | otherwise = storeRegisters ucpu (idx - 1)
                         where ucpu = setMemory cpu (i cpu + idx) (v cpu !! idx)

-- Transfers the copied register values from memory back into (v cpu) starting at position (i cpu).
loadRegisters :: CPU -> Int -> CPU
loadRegisters cpu idx | idx == 0  = ucpu
                      | otherwise = loadRegisters ucpu (idx - 1)
                        where ucpu = setRegister cpu idx (memory cpu !! (i cpu + idx))

{- generateRandomValue cpu idx andVal
   Sets a register to a random value.

   PRE: andVal > 0, idx is a single hexadecimal number.
   RETURNS: cpu where (v cpu !! idx) has been set to ((a random number) .&. andVal)
   EXAMPLES: generateRandomValue cpu 0x0 0x25 = (cpu where (v cpu !! 0x0)) = (random number) .&. 0x25
             generateRandomValue cpu 0xA 0x8C = (cpu where (v cpu !! 0xA)) = (random number) .&. 0x8C
-}
generateRandomValue :: CPU -> Int -> Int -> CPU
generateRandomValue cpu idx andVal = 
  let
    (randomValue, newStdGen) = randomR (0, 255) (rgen cpu)
    ucpu = cpu {rgen = newStdGen}
  in setRegister ucpu idx (randomValue .&. andVal)

{- checkIfInput cpu regIndex
   Checks if any keys are pressed and if so sets the value of a register to the 
   index of the first pressed key.

   PRE: regIndex is a single hexadecimal number.
   RETURNS: unaltered cpu if there are no registered keys, otherwise cpu where (v cpu) !! regIndex
            has been set to the index of the first registered key.
   EXAMPLES: checkIfInput (cpu where number 1 is the only key pressed)         0x6 = (cpu where (v cpu) !! 0x6 = 0x0)
             checkIfInput (cpu where number 3 and 'q' were pressed)            0x6 = (cpu where (v cpu) !! 0x6 = 0x2)
             checkIfInput (cpu where character 'q' is the only key pressed)    0x6 = (cpu where (v cpu) !! 0x6 = 0x3)

-}
checkIfInput :: CPU -> Int -> CPU
checkIfInput cpu reg | null $ filter (==True) (keyboard cpu) = cpu
                     | otherwise = incPC $ setRegister cpu reg (findIndexTrue (keyboard cpu) 0)
                       where
                         findIndexTrue (x:xs) idx | x = idx
                                                  | otherwise = findIndexTrue xs (idx + 1)

{- drawSprite cpu x y times
   Flips a certain number of pixels in a CPUs vram.

   PRE: times > 0 
   RETURNS: cpu where (vram cpu) has been updated to XOR out pixels starting at
            ((vram cpu) !! y) !! x.
   RETURNS: cpu where the rectangle of pixels that is (width = 8 * length = times) large starting at
            coordinates (x, y) in (vram cpu) has been flipped (0 becomes 1 and 1 becomes 0 (XOR operation)).
-}
drawSprite :: CPU -> Int -> Int -> Int -> CPU
drawSprite cpu x y times = drawSprite' (Util.nestedListIndexes (times-1) 7) (setRegister cpu 0xF 0) x y
  where
    drawSprite' [] cpu _ _ = cpu
    drawSprite' ((col, row):xs) cpu x y = let
                                            y2 = (v cpu !! y + row) `mod` windowHeight
                                            x2 = (v cpu !! x + col) `mod` windowWidth
                                            output = shiftR (memory cpu !! (i cpu + row)) (7 - col) .&. 1
                                          in 
                                            drawSprite' xs (xorVram cpu x2 y2 output) x y

{- xorVram cpu x y value
   Flips a pixel in a CPUs vram.

   PRE: value = 0 or 1
   RETURNS: cpu where ((vram cpu !! y) !! x) = ((vram cpu !! y) !! x) XOR value
   EXAMPLES: xorVram (cpu where vram is [[0,0,0]) 1 1 1 = (cpu where vram is [[0,0,0])
                                         [0,0,0]                              [0,1,0]
                                         [0,0,0]]                             [0,0,0]]
-}
xorVram :: CPU -> Int -> Int -> Int -> CPU
xorVram cpu x y val = 
  let
    oldVal = (vram cpu !! y) !! x
    newRow = Util.replace x (xor oldVal val) (vram cpu !! y) 
    ucpu = cpu {vram = Util.replace y newRow (vram cpu)}
  in 
    setRegister ucpu 0xF (if oldVal == 1 && (vram ucpu !! y) !! x == 0 then 1 else v ucpu !! 0xF)
