-- Assignment 2
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char


-- TODO: Implement
-- parseMessage :: String -> LogMessage

parseMessageType :: String
    -> Maybe MessageType
parseMessageType [] = Nothing
parseMessageType [_] = Nothing -- This shouldn't be the case!!!!
parseMessageType message =
  let (typeof:num:_) = words message in
  let n = read num in
  case typeof of
    "I" -> Just Info
    "W" -> Just Warning
    "E" -> Just (Error n)
--     _  -> Nothing
