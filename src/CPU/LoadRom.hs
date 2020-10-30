
module CPU.LoadRom where 

import System.IO
import qualified Data.ByteString as B

{- readRom path
   Reads a file byte by byte and converts them to a list of integers.

   PRE: path leads to a valid FilePath
   RETURNS: a list of all bytes in the file that path leads to.
   SIDE EFFECTS: reads the file at path, exception thrown if it does not exist
   EXAMPLES: readRom (FilePath with text file containing 4 characters) = [13,10,35,12]
-}
readRom :: FilePath -> IO [Int]
readRom path = do
    file <- B.readFile path
    let binaryList = map fromIntegral (B.unpack file)
    return (checkRom binaryList)


{- checkRom rom
   Ensures that ROM is a valid rom.

   RETURNS: rom
   SIDE EFFECTS: exception thrown if rom is not valid
   EXAMPLES: checkRom (rom larger than 3584) = exception thrown
             checkRom (valid rom)            = rom
-}
checkRom :: [Int] -> [Int]
checkRom rom 
  | length rom > 0xE00 = error "File too large"
  | length rom `mod` 2 /= 0 = error "File contains an odd number of bytes and therefore contains not opcodes."
  | otherwise = rom 
