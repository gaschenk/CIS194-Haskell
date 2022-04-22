{-Validating Credit Card Numbers-}

toDigitsRev :: Int -> [Int]
toDigitsRev 0 = []
toDigitsRev n | n `mod` 10 >= 1 = (n `mod` 10) : toDigitsRev (n `div` 10)
              | otherwise       = 0 : toDigitsRev (n `div` 10)

toDigits :: Int -> [Int]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x : xs)
  | even (length xs - 1) = if x * 2 >= 10
    then toDigits (x * 2) ++ doubleEveryOther xs
    else x * 2 : doubleEveryOther xs
  | otherwise = x : doubleEveryOther xs

sumDigits :: [Int] -> Int
sumDigits []       = 0
sumDigits (x : xs) = x + sumDigits xs

validate :: Int -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

{-Exercise 5 Tower of Hanoi-}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
  let step1 = hanoi (n - 1) a c b
      step2 = (a, b)
      step3 = hanoi (n - 1) c b a
  in  step1 ++ [step2] ++ step3


