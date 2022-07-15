{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parse one log message
parseMessage :: String -> LogMessage
parseMessage []   = Unknown ""
parseMessage x = case head x of
    'I' -> LogMessage
           Info
           (readElemAsTS 1)
           (unwords $ drop 2 seqs)
    'W' -> LogMessage
           Warning
           (readElemAsTS 1)
           (unwords $ drop 2 seqs)
    'E' -> LogMessage
           (Error $ readElemAsInt 1)
           (readElemAsTS 2)
           (unwords $ drop 3 seqs)
    _   -> Unknown x
  where
    seqs :: [String]
    seqs = words x

    readElemAsInt :: Int -> Int
    readElemAsInt n = read $ seqs !! n

    readElemAsTS :: Int -> TimeStamp
    readElemAsTS n = read $ seqs !! n

-- parse a whole log file
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- test it with the first 10 lines of `error.log`
texto :: String
texto = "I 5053 pci_id: con ing!\nI 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)\nW 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled\nI 4076 verse.'\nI 4764 He trusts to you to set them free,\nI 858 your pocket?' he went on, turning to Alice.\nI 898 would be offended again.\nI 3753 pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)\nI 790 those long words, and, what's more, I don't believe you do either!' And\nI 3899 hastily.\n"

-- insert LogMessage in MessageTree sorted by TimeStamp
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ )            mt                                     = mt
insert lm                      Leaf                                   = Node Leaf lm Leaf
insert lm@(LogMessage _ tsm _) (Node mt1 lmt@(LogMessage _ ts _) mt2) = case compare tsm ts of
       GT -> Node mt1             lmt (insert lm mt2)
       _  -> Node (insert lm mt1) lmt mt2

-- build MessageTree from list of LogMessage
build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:[]) = insert x Leaf
build (x:xs) = insert x (build xs)

-- list with some logs to test with
someLogs :: [LogMessage]
someLogs = map parseMessage ["I 5053 pci_id: con ing!", "I 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)", "W 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled", "I 4076 verse.'", "I 4764 He trusts to you to set them free,", "I 858 your pocket?' he went on, turning to Alice.", "I 898 would be offended again."]

-- sorted list by TimeStamp from sorted MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node mt1 lm mt2) = (inOrder mt1) ++ [lm] ++ (inOrder mt2)
