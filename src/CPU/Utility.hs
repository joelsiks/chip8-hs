
module CPU.Utility where

import Data.Bits ((.&.), shift)
import Control.Lens ((.~), element)

{- splitByte num
   Converts a byte into two 4-bit values.

   PRE: 0 <= num <= 255
   RETURNS: (0xA, 0xB) where input is 0xAB
   EXAMPLES: splitByte 0xAF = (0xA, 0xF)
             splitByte 0xB3 = (0xB, 0x3)
             splitByte 0x12 = (0x1, 0x2)
             splitByte 0x1 = (0x0, 0x1)
-}
splitByte :: Int -> (Int, Int)
splitByte num = (n1, n2)
  where
    n1 = shift ((.&.) num 0xF0) (-4)
    n2 = (.&.) num 0xF

{- replace idx val list
   Replaces a value at a given index in a list with another value.

   PRE: 0 <= idx < length list
   RETURNS: list where the value at idx has been replaced with val.
   EXAMPLES: replace 0 3 [1,2,3] = [3,2,3]
             replace 1 3 [1,1,1] = [1,3,1]
-}
replace :: Int -> a -> [a] -> [a]
replace idx val = element idx .~ val

{- nestedListIndexes rows cols
   Calculates all possible indexes from a pair of rows and cols.

   PRE: rows >= 0 && cols >= 0
   RETURNS: [(x,y) | x <- [0..cols], y <- [0..rows]]
   EXAMPLES: nestedListIndexes 1 1 = [(0,0),(0,1),(1,0),(1,1)]
             nestedListIndexes 1 2 = [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)]
-}
nestedListIndexes :: Int -> Int -> [(Int, Int)]
nestedListIndexes rows cols = [(col, row) | col <- [0..cols], row <- [0..rows]]
