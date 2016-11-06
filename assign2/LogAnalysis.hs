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
insert l Leaf = Node Leaf l Leaf
insert l (Node left h right)
  | l `isNewer` h = Node (insert l left) h right
  | h `isNewer` l = Node left h (insert l right)

build :: [LogMessage] -> MessageTree
build l = foldr insert Leaf l

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder tree =
  case tree of
    Leaf -> []
    Node left l right -> inOrder left ++ [l] ++ inOrder right

-- begin actual parsing of stuff
priority :: LogMessage -> Maybe Int
priority l =
  case l of
    Unknown _ -> Nothing
    LogMessage ty _ _ ->
      case ty of
        Error n -> Just n
        _ -> Nothing

infix `priorityGreaterThan`
priorityGreaterThan :: Int -> LogMessage -> Bool
priorityGreaterThan n t =
  case priority t of
    Just e_num -> n <= e_num
    Nothing -> False

whatWentWrong :: Int -> [LogMessage] -> [String]
whatWentWrong _ [] = [""]
whatWentWrong n messages = 
  map show $ filter (priorityGreaterThan n) list
  where list = (inOrder . build) messages

