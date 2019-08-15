{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Data.Maybe (isJust, fromJust)
import Text.Read ( readMaybe )
import Log

-- Exercise 1

-- Helpers
safeIntReader :: String -> Maybe Int
safeIntReader = readMaybe

getIntFromJust :: String -> Int 
getIntFromJust s = fromJust . safeIntReader $ s 

isJustValue :: String -> Bool
isJustValue s = isJust . safeIntReader $ s

parseMessage :: String -> LogMessage
parseMessage s = case words s of 
    ("E": level : timestamp : msg)  -> 
      if (and [isJustValue level, isJustValue timestamp])
      then LogMessage (Error . getIntFromJust $ level) (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    ("I": timestamp : msg) ->
      if (isJustValue timestamp)
      then LogMessage Info (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    ("W": timestamp : msg) ->
      if (isJustValue timestamp)
      then LogMessage Warning (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    (_) -> Unknown s

parse :: String -> [LogMessage]
parse [] = []
parse file = map parseMessage $ lines file

