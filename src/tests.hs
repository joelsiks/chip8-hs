
import Test.HUnit
import CPU.CPU as CPU
import CPU.Emulate as Emulate
import CPU.Utility as Util
import CPU.LoadRom as LoadRom
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import System.Random (mkStdGen, randomR)

standardGen = mkStdGen 0
blankCPU = startCPU $ initCPU [0,0,0,0] standardGen

-------------------------------- 
-- CPU Tests

-- setKey
testSetKey = let keyboard = replicate 16 False
             in TestCase $ assertEqual "setKey" True ((CPU.setKey 'a' True keyboard) !! 7)

--initCPU
-- Timers should always be >= 0
testICPU1 = let cpu = CPU.initCPU [1,2,3,4] (mkStdGen 0)
            in TestCase $ assertBool "initCPU [1,2,3,4] (mkStdGen 0)" 
                 (sound_timer cpu >= 0 && delay_timer cpu >= 0)

-- Memory size is constant
testICPU2 = let cpu = CPU.initCPU [1,2,3,4] (mkStdGen 0)
            in TestCase $ assertEqual "initCPU [1,2,3,4] (mkStdGen 0)" 4096 (length (memory cpu))

-- Vram should be blank and a constant size of 64*32
testICPU3 = let cpu = CPU.initCPU [1,2,3,4] (mkStdGen 0)
            in TestCase $ assertEqual "initCPU [1,2,3,4] (mkStdGen 0)" 
                 (replicate (64*32) 0) (concat (vram cpu))

-- Check that running flag is set correctly
testSCPU = let cpu = CPU.initCPU [1,2,3,4] (mkStdGen 0)
           in TestCase $ assertEqual "startCPU cpu" True (isRunning (CPU.startCPU cpu))

-- initMemory
-- Check that memory is the correct size
testIM1 = TestCase $ assertEqual "initMemory [1,2,3,4]" 4096 (length (CPU.initMemory [1,2,3,4]))
-- Check that fontset loaded correctly
testIM2 = TestCase $ assertEqual "initMemory [1,2]" 0x90 (CPU.initMemory [1,2] !! 2)
-- Check that program loaded at correct index
testIM3 = TestCase $ assertEqual "initMemory [0xA,0xB,0xC,0xD]" 0xB (CPU.initMemory [0xA,0xB,0xC,0xD] !! 513)

-- padRom
-- Program with padding should be constant size 
testPR = TestCase $ assertEqual "padRom [1]" 3584 (length (CPU.padRom [1]))

cpuTests = TestList [testSetKey, testICPU1, testICPU2, testICPU3, testSCPU, testIM1, testIM2, testIM3, testPR]

--------------------------------
-- Rom Tests

-- checkRom
testCR1 = TestCase $ assertEqual "checkRom [1,2,3,4]" [1,2,3,4] (LoadRom.checkRom [1,2,3,4])
testCR2 = TestCase $ assertEqual "checkRom ([4097 zeros])" 0 (length (LoadRom.checkRom (replicate 0 4097)))

romTests = TestList [testCR1, testCR2]

-------------------------------- 
-- Utility tests

-- splitByte
testSB1 = TestCase $ assertEqual "splitByte 0xAF" (0xA, 0xF) (Util.splitByte 0xAF)

testSB2 = TestCase $ assertEqual "splitByte 0x13" (0x1, 0x3) (Util.splitByte 0x13)

testSB3 = TestCase $ assertEqual "splitByte 0x0"  (0, 0) (Util.splitByte 0x0)

-- replace
testREP1 = TestCase $ assertEqual "replace 0 0 [1..4]" [0,2,3,4] (Util.replace 0 0 [1..4])

-- nestedListIndexes
testNLI1 = TestCase $ assertEqual "nestedListIndexes 1 1" 
               [(0,0),(0,1),(1,0),(1,1)] (Util.nestedListIndexes 1 1)

utilityTests = TestList [testSB1, testSB2, testSB3, testREP1, testNLI1]

-------------------------------- 
-- Opcode tests

-- 0x00E0: Clears the screen.
testOP01 = TestCase $ assertEqual "opcode 0x00E0" CPU.defaultVRAM (vram (Emulate.executeOpcode blankCPU (0x0,0x0,0xE,0x0)))
-- 0x00EE: Returns from a subroutine.
testOP02 = testRetSub
-- 0x1NNN: Jumps to address NNN.
testOP03 = TestCase $ assertEqual "opcode 0x1NNN" 0x123 (pc (Emulate.executeOpcode blankCPU (0x1,0x1,0x2,0x3)))
-- 0x2NNN: Calls subroutine at NNN.
testOP04 = let ucpu = Emulate.executeOpcode blankCPU (0x2,0x1,0x2,0x3)
           in TestList 
           [ TestCase $ assertEqual "opcode 0x2NNN correct PC" 0x123 (pc ucpu)
           , TestCase $ assertEqual "opcode 0x2NNN correct SP" 0x1 (sp ucpu)
           , TestCase $ assertEqual "opcode 0x2NNN correct Stack value" (pc blankCPU + 2) (stack ucpu !! (sp ucpu - 1))
           ]
-- 0x3XNN: Skips the next instruction if Vx == NN.
testOP05 = TestCase $ assertEqual "opcode 3XNN" (pc blankCPU + 4) (pc (Emulate.executeOpcode blankCPU (0x3,0x0,0x0,0x0)))

-- 0x4XNN: Skips the next instruction if Vx /= NN.
testOP06 = TestCase $ assertEqual "opcode 4XNN" (pc blankCPU + 4) (pc (Emulate.executeOpcode blankCPU (0x4,0x0,0xF,0xF)))

-- 0x5XY0: Skips the next instruction if Vx == Vy
testOP07 = TestCase $ assertEqual "opcode 5XY0" (pc blankCPU + 4) (pc (Emulate.executeOpcode blankCPU (0x5,0x0,0x2,0x0)))

-- 0x6XNN: Sets Vx to NN.
testOP08 = TestCase $ assertEqual "opcode 0x6XNN" 0xFA (v (Emulate.executeOpcode blankCPU (0x6,0x0,0xF,0xA)) !! 0x0)

-- 0x7XNN: Adds NN to Vx.
testOP09 = TestCase $ assertEqual "opcode 0x7XNN" 0x12 (v (Emulate.executeOpcode blankCPU (0x7,0x0,0x1,0x2)) !! 0x0)

-- 0x8XY0: Set Vx to the value of Vy.
testOP10 = let ucpu = blankCPU {v = Util.replace 4 0xA (v blankCPU)}
           in TestCase $ assertEqual "opcode 0x8XY0" 0xA (v (Emulate.executeOpcode ucpu (0x8,0x0,0x4,0x0)) !! 0x0)

-- 0x8XY1: Set Vx to (Vx OR Vy)
testOP11 = let ucpu = blankCPU {v = Util.replace 4 0x2 (replicate 16 0xA)}
           in TestCase $ assertEqual "opcode 0x8XY1" (0xA .|. 0x2) (v (Emulate.executeOpcode ucpu (0x8,0x0,0x4,0x1)) !! 0x0)

-- 0x8XY2: Set Vx to (Vx AND Vy)
testOP12 = let ucpu = blankCPU {v = Util.replace 4 0x2 (replicate 16 0xA)}
           in TestCase $ assertEqual "opcode 0x8XY2" (0xA .&. 0x2) (v (Emulate.executeOpcode ucpu (0x8,0x0,0x4,0x2)) !! 0x0)

-- 0x8XY3: Set Vx to (Vx XOR Vy)
testOP13 = let ucpu = blankCPU {v = Util.replace 4 0x2 (replicate 16 0xA)}
           in TestCase $ assertEqual "opcode 0x8XY3" (xor 0xA 0x2) (v (Emulate.executeOpcode ucpu (0x8,0x0,0x4,0x3)) !! 0x0)

-- 0x8XY4: Adds Vy to Vx. Vf is set to 1 when there's a carry, and to 0 when there isn't. 
testOP14_1 = let ucpu = Emulate.executeOpcode (blankCPU {v = replicate 16 250}) (0x8,0x0,0x3,0x4)
             in TestList
             [ TestCase $ assertEqual "opcode 0x8XY4 with carry right reg value" ((250 + 250) `mod` 256) (v ucpu !! 0x0)
             , TestCase $ assertEqual "opcode 0x8XY4 with carry right carry value" 1 (v ucpu !! 0xF)
             ]

testOP14_2 = let ucpu = Emulate.executeOpcode (blankCPU {v = replicate 16 4})   (0x8,0x0,0x3,0x4)
             in TestList
             [ TestCase $ assertEqual "opcode 0x8XY4 without carry right reg value" 8 (v ucpu !! 0x0)
             , TestCase $ assertEqual "opcode 0x8XY4 without carry right carry value" 0 (v ucpu !! 0xF)
             ]

-- 0x8XY5: Vy is subtracted from Vx. Vf is set to 0 when there's a borrow, and 1 when there isn't. 
testOP15_1 = let ucpu = Emulate.executeOpcode (blankCPU {v = Util.replace 0 10 (replicate 16 5)}) (0x8,0xA,0x0,0x5)
             in TestList
             [ TestCase $ assertEqual "opcode 0x8XY5 with borrow right reg value" 251 (v ucpu !! 0xA)
             , TestCase $ assertEqual "opcode 0x8XY5 with borrow right carry value" 0 (v ucpu !! 0xF)
             ]

testOP15_2 = let ucpu = Emulate.executeOpcode (blankCPU {v = replicate 16 4}) (0x8,0xA,0x0,0x5)
             in TestList
             [ TestCase $ assertEqual "opcode 0x8XY5 without borrow right reg value" 0 (v ucpu !! 0xA)
             , TestCase $ assertEqual "opcode 0x8XY5 without borrow right carry value" 1 (v ucpu !! 0xF)
             ]

-- 0x8XY6: Stores the least significant bit of Vx in Vf and then shifts Vx to the right by 1.
testOP16 = let 
             ucpu = blankCPU {v = replicate 16 10}
             ecpu = Emulate.executeOpcode ucpu (0x8,0x0,0x0,0x6)
           in TestList 
           [ TestCase $ assertEqual "opcode 0x8XY6 shift value" 5 (v ecpu !! 0x0)
           , TestCase $ assertEqual "opcode 0x8XY6 LSB value" 0 (v ecpu !! 0xF)
           ]

-- 0x8XY7: Sets Vx to Vy minus Vx. Vf is set to 0 when there's a borrow, and 1 when there isn't. 
testOP17 = let
             ucpu = blankCPU {v = Util.replace 1 15 (replicate 16 10)}
             ecpu = Emulate.executeOpcode ucpu (0x8,0x1,0x0,0x7)
           in TestList
           [ TestCase $ assertEqual "opcode 0x8XY7 right wrapping value" 251 (v ecpu !! 0x1)
           , TestCase $ assertEqual "opcode 0x8XY7 right borrow" 0 (v ecpu !! 0xF)
           ]

-- 0x8XYE: Stores the most significant bit of VX in VF and then shifts VX to the left by 1.
testOP18 = let
             ucpu = blankCPU {v = replicate 16 10}
             ecpu = Emulate.executeOpcode ucpu (0x8,0x0,0x0,0xE)
           in TestList
           [ TestCase $ assertEqual "opcode 0x8XYE right shift value" 20 (v ecpu !! 0x0)
           , TestCase $ assertEqual "opcode 0x8XYE right MSB value" 0 (v ecpu !! 0xF)
           ]

-- 0x9XY0: Skips the next instruction if Vx /= Vy.
testOP19 = let ucpu = blankCPU {v = replicate 16 0}
           in TestCase $ assertEqual "opcode 0x9XY0" (pc blankCPU + 2) (pc (Emulate.executeOpcode ucpu (0x9,0x0,0x1,0x0)))

-- 0xANNN: Sets I to NNN.
testOP20 = TestCase $ assertEqual "opcode 0xANNN" 0xABC (i (Emulate.executeOpcode blankCPU (0xA,0xA,0xB,0xC)))

-- 0xBNNN: Jumps to the address V0 + NNN.
testOP21 = let ucpu = blankCPU {v = Util.replace 0 0xC (replicate 16 0)}
           in TestCase $ assertEqual "opcode 0xBNNN" (0xC + 0x123) (pc (Emulate.executeOpcode ucpu (0xB,0x1,0x2,0x3)))

-- 0xCXNN: Sets Vx to (a random value between 0-255) AND NN.
testOP22 = let
             (randVal, _) = randomR (0, 255) standardGen
             ucpu = Emulate.executeOpcode blankCPU (0xC,0x0,0xF,0xF)
          in
            TestCase $ assertEqual "opcode 0xCXNN" (randVal .&. 0xFF) (v ucpu !! 0x0)

-- 0xDXYN: Draws a sprite at coordinate (Vx, Vy) that has a width of 8 pixels and a height of N pixels. 
testOP23 = let 
             ucpu = blankCPU {i = 0, vram = replicate 8 $ replicate 8 1}
             ecpu = Emulate.executeOpcode ucpu (0xD,0x0,0x0,0x3)
           in TestList 
           [ TestCase $ assertEqual "opcode 0xDXYN (0xD,0x0,0x0,0x3) row 1" [0,0,0,0,1,1,1,1] $ vram ecpu !! 0
           , TestCase $ assertEqual "opcode 0xDXYN (0xD,0x0,0x0,0x3) row 2" [0,1,1,0,1,1,1,1] $ vram ecpu !! 1
           , TestCase $ assertEqual "opcode 0xDXYN (0xD,0x0,0x0,0x3) row 3" [0,1,1,0,1,1,1,1] $ vram ecpu !! 2
           ]

-- 0xEX9E: Skips the next instruction if the key stored in Vx is pressed.
testOP24 = let ucpu = blankCPU {v = Util.replace 0x0 0xA (replicate 16 0), keyboard = Util.replace 0xA True (replicate 16 False)}
           in TestCase $ assertEqual "opcode 0xEX9E" (pc blankCPU + 4) (pc (Emulate.executeOpcode ucpu (0xE,0x0,0x9,0xE)))

-- 0xEXA1: Skips the next instruction if the key stored in Vx is not pressed.
testOP25 = TestCase $ assertEqual "opcode 0xEXA1" (pc blankCPU + 4) (pc (Emulate.executeOpcode blankCPU (0xE,0x0,0xA,0x1)))

-- 0xFX07: Sets Vx to the value of the delay_timer.
testOP26 = let ucpu = blankCPU {delay_timer = 0xB}
           in TestCase $ assertEqual "opcode 0xFX07" 0xB (v (Emulate.executeOpcode ucpu (0xF,0x0,0x0,0x7)) !! 0x0)

-- 0xFX0A: A key press is awaited, and then stored in Vx. The emulator will only
--         step to the next instruction if a key has been pressed, essentially
--         halting the emulator.
testOP27 = let ucpu = blankCPU {keyboard = Util.replace 0x0 True (replicate 16 False)}
           in TestList
           [ TestCase $ assertEqual "opcode 0xFX0A not next instruction if no key pressed" (pc blankCPU) (pc (Emulate.executeOpcode blankCPU (0xF,0x0,0x0,0xA)))
           , TestCase $ assertEqual "opcode 0xFX0A next instruction if key pressed" (pc blankCPU + 2) (pc (Emulate.executeOpcode ucpu (0xF,0x0,0x0,0xA)))
           , TestCase $ assertEqual "opcode 0xFX0A correct key index stored in reg" 0x0 (v (Emulate.executeOpcode ucpu (0xF,0x0,0x0,0xA)) !! 0x0)
           ]

-- 0xFX15: Sets the delay timer to Vx.
testOP28 = let ucpu = blankCPU {v = Util.replace 0 0xF (replicate 16 0)}
           in TestCase $ assertEqual "opcode 0xFX15" 0xF (delay_timer (Emulate.executeOpcode ucpu (0xF,0x0,0x1,0x5)))

-- 0xFX18: Sets the sound timer to Vx.
testOP29 = let ucpu = blankCPU {v = Util.replace 0 0xF (replicate 16 0)}
           in TestCase $ assertEqual "opcode 0xFX18" 0xF (sound_timer (Emulate.executeOpcode ucpu (0xF,0x0,0x1,0x8)))

-- 0xFX1E: Adds Vx to I. Vf is set to 1 when there is a range overflow (I+Vx > 0xFFF), and to 0 when there isn't.
testOP30 = let 
             ucpu = blankCPU {v = Util.replace 0 0xF (replicate 16 0), i = 0xFFF}
             ecpu = Emulate.executeOpcode ucpu (0xF,0x0,0x1,0xE)
           in TestList
           [ TestCase $ assertEqual "opcode 0xFX1E correct I value" 4110 (i ecpu)
           , TestCase $ assertEqual "opcode 0xFX1E correct overflow flag" 1 (v ecpu !! 0xF)
           ]

-- 0xFX29: Sets I to 5 * Vx.
testOP31 = let ucpu = blankCPU {v = Util.replace 0 0xA (replicate 16 0)}
           in TestCase $ assertEqual "opcode 0xFX29" 50 (i (Emulate.executeOpcode ucpu (0xF,0x0,0x2,0x9)))

-- 0xFX33: Stores the binary coded decimal representation of Vx in memory.
testOP32 = let 
             ucpu = blankCPU {v = Util.replace 0 255 (replicate 16 0)}
             ecpu = Emulate.executeOpcode ucpu (0xF,0x0,0x3,0x3)
             mem = memory ecpu
           in
             TestCase $ assertBool "opcode 0xFX33 correct BCD representation" (mem !! i ecpu == 2 && mem !! (i ecpu + 1) == 5 && mem !! (i ecpu + 2) == 5)

-- 0xFX55: Stores V0 to VX (including VX) in memory starting at address I. 
--         The offset from I is increased by 1 for each value written, but I itself is left unmodified.
testOP33 = let
             ucpu = blankCPU {i = 0, v = replicate 16 1}
             ecpu = Emulate.executeOpcode ucpu (0xF,0xF,0x5,0x5)
           in
             TestCase $ assertEqual "opcode 0xFX55" (v ecpu) (take (0xF+1) (memory ecpu))

-- 0xFX65: Fills V0 to VX (including VX) with values from memory starting at address I. 
--         The offset from I is increased by 1 for each value written, but I itself is left unmodified.
testOP34 = let ucpu = blankCPU {i = 0, memory = [1,2,3] ++ replicate 4093 0}
           in TestCase $ assertEqual "opcode 0xFX65" [1,2,3] (take 3 (v $ Emulate.executeOpcode ucpu (0xF,0x2,0x6,0x5)))

opcodeTests = TestList [ testOP01, testOP02, testOP03, testOP04, testOP05, testOP06, testOP07, testOP08, testOP09, testOP10
                       , testOP11, testOP12, testOP13, testOP14_1, testOP14_2, testOP15_1, testOP15_2, testOP16, testOP17
                       , testOP18, testOP19, testOP20, testOP21, testOP22, testOP23, testOP24, testOP25, testOP26, testOP27
                       , testOP28, testOP29, testOP30, testOP31, testOP32, testOP33, testOP34
                       ]

-------------------------------- 
-- Emulate Tests

-- Tests that value is stored and timer is decresed
testCycle = 
  let cpu = Emulate.emulateCycle (CPU.initCPU [0x6A,0x02] (mkStdGen 0)) {delay_timer = 9}
  in TestCase $ assertBool "initCPU [0x6A,0x02] (mkStdGen 0)" (v cpu !! 0xA == 2 && delay_timer cpu == 8)

-- Sound timer should be decresed but not delay_timer
testTimers =
  let cpu = decreseTimers $ CPU.initCPU [1,2,3] (mkStdGen 0)
      cpu2 = decreseTimers $ cpu {sound_timer = 7, delay_timer = 0}
  in TestCase $ assertBool "decreseTimers cpu" (sound_timer cpu2 == 6 && delay_timer cpu2 == 0)

-- fetchOpcode
-- Fetching a "fake" opcode from the start of the memory where the fontset is located should always yield the same result.
testFOp1 = TestCase $ assertEqual "fetchOpcode" (0xF, 0x0, 0x9, 0x0) 
               (Emulate.fetchOpcode (blankCPU {pc = 0}))

-- opNNN
testOPNNN = TestCase $ assertEqual "opNNN (0xA, 0xB, 0xC, 0xD)" 
                0xBCD (Emulate.opNNN (0xA, 0xB, 0xC, 0xD))

-- opNN
testOPNN = TestCase $ assertEqual "opNN (0x1, 0x2, 0x3, 0x4)" 
               0x34 (Emulate.opNN (0x1, 0x2, 0x3, 0x4))

-- incPC
-- When increasing the program counter it should always be greater than it was before. Exact implementation may vary.
testIncPC = TestCase $ assertBool "incPC" 
                (pc (Emulate.incPC blankCPU) > pc blankCPU)

-- jumpToAddress
testJump = TestCase $ assertEqual "jumpToAddress 0x300" 
               0x300 (pc (Emulate.jumpToAddress blankCPU 0x300))

-- insertToStack
testInsertStack = 
    let
      value = 0xFF
      ucpu = Emulate.insertToStack blankCPU value
    in
      TestCase $ assertEqual ("insertToStack blankCPU " ++ show value)
        value (stack ucpu !! (sp ucpu - 1))

-- returnFromSubroutine
-- When returning from a subroutine the program counter should be the same as it was but incremented by one instruction.
testRetSub =
  let
    callAddr = 0x200
    ucpu = Emulate.returnFromSubroutine (Emulate.callSubroutine blankCPU callAddr)
  in
    TestCase $ assertEqual ("returnFromSubroutine (cpu that has been called to " ++ show callAddr ++ ")")
      (pc (incPC blankCPU)) (pc ucpu)

-- skipInstructionIf
testSkipIf1 = TestCase $ assertEqual "skipInstructionIf blankCPU False"
                  (pc blankCPU) (pc (skipInstructionIf blankCPU False))

testSkipIf2 = TestCase $ assertEqual "skipInstructionIf blankCPU True"
                  (pc (Emulate.incPC blankCPU)) (pc (skipInstructionIf blankCPU True))

-- setRegister
-- Checks if the correct value has been inserted at the correct index.
testSetReg =
  let
    index = 0xA
    value = 0xFF
    ucpu = setRegister blankCPU index value
  in
    TestCase $ assertEqual ("setRegister blankCPU "  ++ show index ++ " " ++ show value)
      value (v ucpu !! index)

-- setMemory
-- Checks if the correct value has been inserted at the correct index.
testSetMem =
  let
    index = 0xA
    value = 0xFF
    ucpu = setMemory blankCPU index value
  in
    TestCase $ assertEqual ("setMemory blankCPU "  ++ show index ++ " " ++ show value)
      value (memory ucpu !! index)

-- storeBCDRepresentation
-- When constructing the value from the memory it should be the same as it was when passed.
testStoreBCD =
  let 
    index = 0x0
    value = 0xFF
    ucpu1 = blankCPU {v = Util.replace index value (v blankCPU)}
    ucpu2 = Emulate.storeBCDRepresentation ucpu1 index
  in
    TestCase $ assertEqual ("storeBCDRepresentation blankCPU " ++ show index)
      value ((memory ucpu2 !! i ucpu2) * 100 + (memory ucpu2 !! (i ucpu2 + 1)) * 10 + memory ucpu2 !! (i ucpu2 + 2))

-- storeRegisters
-- The correct amount of registers should be copied to the memory and with correct values.
testStoreReg = 
  let
    amount = 0xF
    ucpu1 = blankCPU {i = 0, v = [0..15]}
    ucpu2 = storeRegisters ucpu1 amount
  in
    TestCase $ assertEqual ("storeRegisters (blankCPU where i=0 & v = [0..15]) " ++ show amount)
      (v ucpu1) (take (amount+1) (memory ucpu2))

-- loadRegisters
-- The correct amount of registers should be copied from the memory and with correct values.
testLoadReg = 
  let
    amount = 0xA
    ucpu1 = blankCPU {i = 0}
    ucpu2 = loadRegisters ucpu1 amount
  in
    TestCase $ assertEqual ("loadRegisters (blankCPU where i=0 & v = [0..15]) " ++ show amount)
      (take amount CPU.fontset) (take amount (v ucpu2))

-- generateRandomValue
-- blankCPU uses the same standardGen as in this test so they should yield the same result when generating
-- a random number.
testRandVal =
  let
    (randVal, _) = randomR (0, 255) standardGen
    andVal  = 0x3A
    index = 0x0
    ucpu = Emulate.generateRandomValue blankCPU index andVal
  in
    TestCase $ assertEqual ("generateRandomValue blankCPU " ++ show index ++ " " ++ show andVal)
      (v ucpu !! index) (randVal .&. andVal)

-- checkIfInput
testCheckInput1 =
  let
    keyIndex = 0x3
    regIndex = 0x6
    ucpu = blankCPU {keyboard = Util.replace keyIndex True (replicate 16 False)}
  in
    TestCase $ assertEqual ("checkIfInput (cpu where keyboard index " ++ show keyIndex ++ " set to True) " ++ show regIndex)
      (v (checkIfInput ucpu regIndex) !! regIndex) keyIndex

testCheckInput2 = TestCase $ assertEqual "checkIfInput (cpu where every key set to False) 0x0"
                      (keyboard (checkIfInput blankCPU 0x0)) (replicate 16 False)

-- shiftRegLeft
testShiftLeft =
  let
    regIndex = 0x4
    n1 = 224
    n2 = 20
    ucpu1 = setRegister blankCPU regIndex n1
    ucpu2 = setRegister blankCPU regIndex n2
  in           -- Tests if 8-bit overflow "wraps" around correctly.
    TestList [ TestCase $ assertEqual ("shiftRegLeft (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n1 ++ ") " ++ show regIndex)
                  (v (shiftRegLeft ucpu1 regIndex) !! regIndex) 192
               -- Tests normal use case.
             , TestCase $ assertEqual ("shiftRegLeft (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n2 ++ ") " ++ show regIndex)
                 (v (shiftRegLeft ucpu2 regIndex) !! regIndex) 40
             ]

-- shiftRegRight
testShiftRight =
  let
    regIndex = 0x4
    n1 = 224
    n2 = 20
    ucpu1 = setRegister blankCPU regIndex n1
    ucpu2 = setRegister blankCPU regIndex n2
  in
    TestList [ TestCase $ assertEqual ("shiftRegRight (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n1 ++ ") " ++ show regIndex)
                 (v (shiftRegRight ucpu1 regIndex) !! regIndex) 112
             , TestCase $ assertEqual ("shiftRegRight (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n2 ++ ") " ++ show regIndex)
                 (v (shiftRegRight ucpu2 regIndex) !! regIndex) 10
             ]

-------------------------------- 
-- Cli Tests

-- getFPS
testGetFPS = TestList [TestCase $ assertEqual "getFPS for PONG" (getFPS "PONG") 300,
                       TestCase $ assertEqual "getFPS for BLINKY" (getFPS "BLINKY") 600,
                       TestCase $ assertEqual "getFPS for undefined rom" (getFPS "X") 100]

-- buildString
testBuildString = TestCase $ assertEqual "buildString for [jag,hello,str]" (buildString ["jag","hello","str"]) "jag, hello, str"

-------------------------------- 
emulateTests = TestList [ testCycle, testTimers, testOPNNN, testOPNN, testFOp1, testIncPC, testJump, testInsertStack, testRetSub
                        , testSkipIf1, testSkipIf2, testSetReg, testSetMem, testStoreBCD, testStoreReg
                        , testLoadReg, testRandVal, testCheckInput1, testCheckInput2, testShiftLeft
                        , testShiftRight, testGetFPS, testBuildString
                        ]

-------------------------------- 

runtests = runTestTT $ TestList [cpuTests, romTests, utilityTests, emulateTests, opcodeTests]
