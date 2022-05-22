{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits x = if x <= 0 then [] else toDigits ((x - xmod10) `div` 10) ++ [xmod10]
    where
        xmod10 = x `mod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if x <= 0 then [] else [xmod10] ++ toDigitsRev ((x - xmod10) `div` 10)
    where
        xmod10 = x `mod` 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:xs) = if ((length xs) `mod` 2) /= 0 then [2 * x] ++ (doubleEveryOther xs) else [x] ++ (doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:[]) = x
sumDigits (x:xs) = sumDigits xs + sum (toDigits x)

validate :: Integer -> Bool
validate x = if (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0 then True else False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = [("", "")]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a

main1 :: IO ()
main1 = do putStrLn (show (toDigits 123456))
           putStrLn (show (toDigitsRev 123456))

main2 :: IO ()
main2 = do putStrLn (show (doubleEveryOther []))
           putStrLn (show (doubleEveryOther [2]))
           putStrLn (show (doubleEveryOther [8,7,6,5]))
           putStrLn (show (doubleEveryOther [1,2,3]))

main3 :: IO ()
main3 = putStrLn (show (sumDigits [16,7,12,5,1]))

main4 :: IO ()
main4 = do putStrLn (show (validate 4012888888881881))
           putStrLn (show (validate 4012888888881882))

main :: IO ()
main = do putStrLn (show (hanoi 0 "a" "b" "c"))
          putStrLn (show (hanoi 1 "a" "b" "c"))
          putStrLn (show (hanoi 2 "a" "b" "c"))
          putStrLn (show (hanoi 3 "a" "b" "c"))
          putStrLn (show (hanoi 4 "a" "b" "c"))