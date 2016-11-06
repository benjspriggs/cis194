-- Assignment 2
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log


-- TODO: Implement
-- parseMessage :: String -> LogMessage

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
