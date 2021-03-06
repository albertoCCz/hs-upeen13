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
-- Examples:
--     skips "ABCD"       == ["ABCD", "BD", "C", "D"]
--     skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
--     skips [1]          == [[1]]
--     skips [True,False] == [[True,False], [False]]
--     skips []           == []
-- =========================================================
-- How it works:
-- 1. [[] | i <- []] : produces a list of lists.
-- 2. We map (x !!) over each of these sublists.
-- 3. For each sublist, we generate a value i from
--    the list [i, 1, 2, ..., (length x) - 1].
-- 4. For each value i, we generate a list where:
--        4.1. The initial value is i.
--        4.2. The step is 2*i + 1.
--        4.3. The final value is always length x.
--    This list is later use as one of the list of indexes.
-- Example:
--     x = "ABCD"
--     skips x = [map ("ABCD" !!) [i,2*i+1..3] | i <- [0,1,2,3]] =
--             = [map ("ABCD" !!) [0,2*0+1..3],
--                map ("ABCD" !!) [1,2*1+1..3],
--                map ("ABCD" !!) [2,2*2+1..3],
--                map ("ABCD" !!) [3,2*3+1..3]] =
--             = [map ("ABCD" !!) [0,1,2,3],
--                map ("ABCD" !!) [1,3,],
--                map ("ABCD" !!) [2],
--                map ("ABCD" !!) [3]] =
--             = [map ("ABCD" !!) [0,1,2,3],
--                map ("ABCD" !!) [1,3],
--                map ("ABCD" !!) [2],
--                map ("ABCD" !!) [3]] =
--             = ["ABCD", "BD", "C", "D"]
skips :: [a] -> [[a]]
skips x = [map (x !!) [i,2*i+1..length x-1] | i <- [0..length x-1]]

-- Exercise 2 Local maxima
-- | @localMaxima x@ takes a list @x@ and returns a list whith
--   the values in @x@ whose neighbours are lower.
-- Examples:
--     localMaxima [2,9,5,6,1] == [9,6]
--     localMaxima [2,3,4,1,5] == [4]
--     localMaxima [1,2,3,4,5] == []
-- =========================================================
-- How it works:
-- It works recursively asking to the input list if it has at
-- least three elements and if the central one is the bigger.
-- Example:
--     x = [2,9,5,6,1]
--     localMaxima x = localMaxima (2:9:5:[6,1])   =
--                   = 9 : localMaxima (9:5:[6,1]) =
--                   = 9 : localMaxima (5:6:[1])   =
--                   = 9 : 6 : localMaxima []      =
--                   = 9 : 6 : [] = [9,6]
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = if (y > x) && (y > z)
                         then y : localMaxima (y:z:xs)
                         else localMaxima (y:z:xs)
localMaxima _          = []