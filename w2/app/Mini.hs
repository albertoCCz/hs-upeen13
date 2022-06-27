{- Module with some of the things I've been
   trying along the way (just so I don't repeat
   them). -}
module Mini where

import Log
import Data.List (findIndices)

lee :: String -> Int
lee = read

-- parse one message without using `words` function
parseMessage' :: String -> LogMessage
parseMessage' []   = Unknown ""
parseMessage' (x:xs) = case x of
    'I' -> LogMessage
           Info
           (readElemAsTS 1 xs)
           (drop (wsIndices !! 1 + 1) xs)
    'W' -> LogMessage
           Warning
           (readElemAsTS 1 xs)
           (drop (wsIndices !! 1 + 1) xs)
    'E' -> LogMessage
           (Error $ readElemAsInt 1 xs)
           (readElemAsTS 2 xs)
           (drop (wsIndices !! 2 + 1) xs)
    _   -> Unknown (x:xs)
  where
    wsIndices :: [Int]
    wsIndices = findIndices (== ' ') xs

    getSubstrN :: Int -> String -> String
    getSubstrN n s = drop prefixNumber allButSuffix
                where
                    prefixNumber :: Int
                    prefixNumber = wsIndices !! (n - 1) + 1

                    allButSuffix :: String
                    allButSuffix = take (wsIndices !! n) s

    readElemAsInt :: Int -> String -> Int
    readElemAsInt n s = read $ getSubstrN n s

    readElemAsTS :: Int -> String -> TimeStamp
    readElemAsTS n s = read $ getSubstrN n s