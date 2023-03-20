{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read (readMaybe)

-- Exercise 1 - Parse a string to give a LogMessage type
isNumber :: String -> Bool -- or all isNumber '' ... from Data.Char (isNumber)
isNumber n = case (readMaybe n :: Maybe Integer) of
                (Just _) -> True
                Nothing -> False

checkMsgType :: String -> String -> String -> Maybe MessageType
checkMsgType m lvl_ts ts
    | m == "E" && hasErrLvlAndTS = Just (Error (read lvl_ts))
    | m == "I" && hasTimeStamp = Just Info
    | m == "W" && hasTimeStamp = Just Warning
    | otherwise = Nothing
        where hasErrLvlAndTS = isNumber lvl_ts && isNumber ts
              hasTimeStamp = isNumber lvl_ts

parseMessage :: String -> LogMessage
parseMessage msg = case msgType of 
                    (Just (Error lvl)) -> LogMessage (Error lvl) (read ts) (unwords rest)
                    (Just mt) -> LogMessage mt (read lvl_ts) (unwords (ts : rest))
                    Nothing -> Unknown msg
    where (errCode, rest) = splitAt 3 (words msg)
          lvl_ts = errCode !! 1
          ts = errCode !! 2
          msgType = checkMsgType (errCode !! 0) lvl_ts ts

parse :: String -> [LogMessage]
parse = (map parseMessage).lines

-- Test this function using this funcion: testParse parse 10 "error.log" ... 10 is the no. of lines to test

-- Exercise 2 - insert a LogMessage into a Log MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert (LogMessage m ts s) (Node left (LogMessage mT tsNode sT) right)
    | ts > tsNode = Node left logMsgTree (insert logMsg right)
    | otherwise = Node (insert logMsg left) logMsgTree right
        where logMsg = LogMessage m ts s
              logMsgTree = LogMessage mT tsNode sT
insert _ tree = tree -- This isn't necessary, but Haskell will yell out a warning otherwise :(

-- Exercise 3 - Build a Log MessageTree using a list of LogMessages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4 - Takes a sorted MessageTree and returns the list of LogMessages in that tree (inOrder traversal)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

-- Exercise 5 - Extract relevant LogMessages (with severity >= 50)
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logMsgs = [str | (LogMessage _ _ str) <- filtered]
    where ordered = inOrder $ build logMsgs
          filtered = dropWhile (\(LogMessage _ ts _) -> ts < 50) ordered 