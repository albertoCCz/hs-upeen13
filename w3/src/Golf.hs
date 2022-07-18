module Golf where

-- Exercise 1 Hopscotch
-- | @skips x@ takes a list @x@ and returns a list of lists
--   of the same type as x, where each element is a sublist
--   of @x@ where:
--       - the first element of the first sublist is the
--         first element of @x@, the first elem of the
--         second sublist is the second element of @x@, ...
--       - the step between elements of sublists increments
--         by one, starting at 1, for each sublist.
--   Examples:
--       skips "ABCD"       == ["ABCD", "BD", "C", "D"]
--       skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
--       skips [1]          == [[1]]
--       skips [True,False] == [[True,False], [False]]
--       skips []           == []
skips :: [a] -> [[a]]
skips x = [map (x !!) [i,2*i+1..lenL-1] | i <- [0..lenL-1]]
    where
        lenL :: Int
        lenL = length x
