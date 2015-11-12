import Data.List

smallestNumber :: Maybe Int
smallestNumber = find (\x -> sumDigits x `mod` 10 == 0 && sumDigits (x+1) `mod` 10 == 0) [18999999998..]

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = (n `mod` 10) + sumDigits (n `div` 10)
