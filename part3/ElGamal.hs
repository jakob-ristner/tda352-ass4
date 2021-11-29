{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat) 
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)

import Data.List

-- Some helper functions from part3 of the programming assignment

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Integer, Integer) -> (Integer, Integer, Integer)
eea (a, b) = eea' (a, b, 1, 0, 0, 1)

-- note that xn, xn1, xn2 denote x_n, x_n+1, x_n+2
eea' :: (Integer, Integer, Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer) -- helper function
eea' (rn, rn1, sn, sn1, tn, tn1) 
    | rn2 == 0 = (rn1, sn1, tn1)
    | otherwise = eea' (rn1, rn2, sn1, sn2, tn1, tn2)
        where rn2 = mod rn rn1 
              qn2 = div rn rn1
              sn2 = sn - qn2 * sn1
              tn2 = tn - qn2 * tn1

-- | Returns a^k (mod n).
modExp :: (Integer, Integer, Integer) -> Integer
modExp (_, _, 0) = 1
modExp (n, a, k) | even k = modExp (n, (mod (a^2) n), (div k 2))
                 | odd k  = mod (a * modExp (n, a, (k-1))) n

-- | Returns the value v such that n*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv :: (Integer, Integer) -> Integer
modInv (n, m) | g /= 1 = 0 -- no modular inverse exists
              | otherwise = mod s m
              where (g, s, _) = eea (n, m)

main = do
  let file = "input.txt"
  content <- readFile file
  let (p, g, y, year, month, day, hour, minute, second, c1, c2) = parseInput content
  let m = recoverMessage p g y year month day hour minute second c1 c2
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- | Parses the problem.
parseInput :: String -> (Integer, Integer, Integer, Int, Int, Int, Int, Int, Int, Integer, Integer)
parseInput content =
  let fileLines = take 6 $ lines content
      p  = readOne (fileLines !! 0)
      g  = readOne (fileLines !! 1)
      y  = readOne (fileLines !! 2)
      t = (T.splitOn "=" (T.pack (fileLines !! 3))) !! 1
      date = (T.splitOn " " t) !! 0
      time = (T.splitOn " " t) !! 1
      year   = (read . T.unpack) $ (T.splitOn "-" date) !! 0
      month  = (read . T.unpack) $ (T.splitOn "-" date) !! 1
      day    = (read . T.unpack) $ (T.splitOn "-" date) !! 2
      hour   = (read . T.unpack) $ (T.splitOn ":" time) !! 0
      minute = (read . T.unpack) $ (T.splitOn ":" time) !! 1
      second = (read . T.unpack) $ (T.splitOn ":" time) !! 2
      c1  = readOne (fileLines !! 4)
      c2  = readOne (fileLines !! 5)
  in  (p, g, y, year, month, day, hour, minute, second, c1, c2)
  where readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1


recoverMessage :: Integer -> Integer -> Integer -> Int -> Int -> Int -> Int ->
                  Int -> Int -> Integer -> Integer -> Integer
recoverMessage p g y year month day hour minute second c1 c2 = mod (c2 * inversek) p


    where base = testKeyBase year month day hour minute second
          keys = nub [base + num + diff | num <- [0..999], diff <- [-1..1]]
          skey = head [key | key <- keys, testPrivateKey p g c1 key]
          bigk = modExp (p, y, skey)
          inversek = modInv (bigk, p)

testKeyBase :: Int -> Int -> Int -> Int -> Int -> Int -> Integer
testKeyBase year month day hour minute second =
    toInteger $ year * 10^10 + month * 10^8 + day * 10^6 + hour * 10^4 + minute * 10^2 + second

testPrivateKey :: Integer -> Integer -> Integer -> Integer -> Bool
testPrivateKey p g y test = y == (modExp (p, g, (mod test p)))







