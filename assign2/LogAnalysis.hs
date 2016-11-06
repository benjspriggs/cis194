-- Assignment 2
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Begin parse
parseMessageType :: String
    -> (Maybe MessageType, [String])
parseMessageType [] = (Nothing, [""])
parseMessageType message =
  let (typeof:msg) = words message in
  case typeof of
    "I" -> (Just Info, msg)
    "W" -> (Just Warning, msg)
    "E" -> (Just (Error (read $ head msg)), drop 1 msg)
    _  -> (Nothing, msg)

parseTimeStamp :: [String]
  -> (TimeStamp, [String])
parseTimeStamp [] = (0 :: TimeStamp, [])
parseTimeStamp ss = (read $ head ss, drop 1 ss)

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage message = 
  let (typeof, _msg) = parseMessageType message in
  let (timestamp, msg) = parseTimeStamp _msg in
  case typeof of
    Nothing -> Unknown message
    Just t -> LogMessage t timestamp (unwords msg)

parse :: String -> [LogMessage]
parse "" = []
parse s = map parseMessage (lines s)

-- begin tree stuffs
infix `isNewer`
isNewer :: LogMessage -> LogMessage -> Bool
isNewer la lb = time la > time lb

time :: LogMessage -> Maybe TimeStamp
time l =
  case l of
    Unknown _ -> Nothing
    LogMessage _ times _ -> Just times 

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert l mt =
  case mt of
    Leaf -> Node Leaf l Leaf
    Node left h right ->
      if h `isNewer` l
        then insert l left
        else insert l right

build :: [LogMessage] -> MessageTree
build l = foldr insert Leaf l

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder tree =
  case tree of
    Leaf -> []
    Node left log right -> inOrder left ++ [log] ++ inOrder right
