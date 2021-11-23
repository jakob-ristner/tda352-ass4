module CryptoLib (eea, modExp, modInv, fermatPT) where

--Note: Tests do not compile at all

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Int, Int) -> (Int, Int, Int)
eea (a, b) = eea' (a, b, 1, 0, 0, 1)

-- note that xn, xn1, xn2 denote x_n, x_n+1, x_n+2
eea' :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int) -- helper function
eea' (rn, rn1, sn, sn1, tn, tn1) 
    | rn2 == 0 = (rn1, sn1, tn1)
    | otherwise = eea' (rn1, rn2, sn1, sn2, tn1, tn2)
        where rn2 = mod rn rn1 
              qn2 = div rn rn1
              sn2 = sn - qn2 * sn1
              tn2 = tn - qn2 * tn1

-- | Returns a^k (mod n).
modExp :: (Int, Int, Int) -> Int
modExp (_, _, 0) = 1
modExp (n, a, k) | even k = modExp (n, (mod (a^2) n), (div k 2))
                 | odd k  = mod (a * modExp (n, a, (k-1))) n

-- | Returns the value v such that n*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv :: (Int, Int) -> Int
modInv (n, m) | g /= 1 = 0 -- no modular inverse exists
              | otherwise = mod s m
              where (g, s, _) = eea (n, m)

-- | Returns 0 if n is a Fermat Prime, otherwise it returns the lowest
-- Fermat Witness. Tests values from 2 (inclusive) to n/3 (exclusive).
fermatPT :: Int -> Int
fermatPT n = ft n (div n 3)

ft :: Int -> Int -> Int
ft n 1 = 0
ft n k | modExp (n, k, n-1) == 1 = ft n (k - 1)
       | otherwise = k

 
