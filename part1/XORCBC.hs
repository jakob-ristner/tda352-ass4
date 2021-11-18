import qualified Data.ByteString as B (unpack, pack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Word (Word8)
import Data.Bits (xor) -- imported for bitwise xor

unHex :: String -> [Word8]
unHex [] = []
unHex (x:y:xs) = toEnum (read ['0','x',x,y]) : unHex xs


-- Helper funcitons
blockSize :: Int
blockSize = 12

getBlock :: [Word8] -> Int -> [Word8]
getBlock block index = take blockSize $ drop (blockSize * index) block

xor3block :: [Word8] -> [Word8] -> [Word8] -> [Word8]
xor3block [] [] [] = []
xor3block (c1:cs1) (c0:cs0) (m1:ms1) = (xor c1 (xor c0 m1)):(xor3block cs1 cs0 ms1)

split :: [Word8] -> [[Word8]] -- Splits array of bytes in to blocks
split [] = []
split blocks = (take blockSize blocks):(split $ drop blockSize blocks)

-- Decryption and key calculation

decrypt :: [[Word8]] -> [Word8] -> [Word8]
decrypt [c0, c1] key = []
decrypt (c0:(c1:cs)) key = (xor3block c0 c1 key) ++ (decrypt (c1:cs) key)

key :: [Word8] -> [Word8] -> [Word8] 
key first_block encrypted = xor3block (getBlock encrypted 0) (getBlock encrypted 1) (first_block) -- c0 + c1 + m



main = do
  let file = "input.txt"
  content <- readFile file
  let (first_block, encrypted) = parseInput content
  let m = recoverMessage first_block encrypted 
  putStrLn $ "Recovered message: " ++ show (drop blockSize m)

-- | Parses the problem.
parseInput :: String -> ([Word8], [Word8])
parseInput content =
  let fileLines = lines content
      first_block = B.unpack (BC.pack (fileLines !! 0))
      encrypted = unHex $ fileLines !! 1
  in (first_block, encrypted)

-- | Recover the encrypted message, knowing the first block of plain text. The
-- encrypted text is of the form IV | C0 | C1 | ... where each block is 12 bytes
-- long.
recoverMessage :: [Word8] -> [Word8] -> String
recoverMessage first_block encrypted =
  -- TODO. Decrypt the message on the byte (Word8) representation. When you have
  -- the final message, convert it to a string a shown below.
  BC.unpack (B.pack $ decrypt (split encrypted) (key first_block encrypted))
