-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
    | n < 1 = []
    | otherwise = (toDigits (n `div` 10)) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 1 = []
    | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs)
    | even $ length xs = (x*2) : y : doubleEveryOther xs
    | otherwise = x : (2*y) : doubleEveryOther xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum.concat.(map toDigits)

-- Exercise 4
validate :: Integer -> Bool
validate n = let sumAll = sumDigits $ doubleEveryOther $ toDigits n in (sumAll `mod` 10) == 0