{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCEventLogChrome ( processEventLogs,
                           processEventLog ) where

import Control.Applicative
import GHC.Generics
import GHC.RTS.Events
import GHC.RTS.Events.Merge
import Data.Aeson
import Data.List
import Data.Maybe

emptyEventLog :: EventLog
emptyEventLog = EventLog (Header []) (Data [])

data ChromeEventPhase =
      Begin
    | End
    | Complete

instance ToJSON ChromeEventPhase where
    toJSON End      = String "E"
    toJSON Begin    = String "B"
    toJSON Complete = String "X"

data ChromeEvent = ChromeEvent {
    name :: String,
    cat :: String,
    ph :: ChromeEventPhase,
    ts :: Timestamp,
    pid :: Int,
    tid :: Int
    -- TODO: args
} deriving (Generic)

instance ToJSON ChromeEvent

data ChromeTracing = ChromeTracing {
    traceEvents :: [ChromeEvent]
} deriving (Generic)

instance ToJSON ChromeTracing

toChromeEvent :: Event -> Maybe ChromeEvent
toChromeEvent e = do
        (name, ph) <- parseMarker $ evSpec e
        pure $ ChromeEvent name "" ph (evTime e) 1 1
    where parseMarker :: EventInfo -> Maybe (String, ChromeEventPhase)
          parseMarker (UserMessage markername) =
              ((, Begin) <$> stripPrefix "GHC:started: " markername) <|>
              ((, End) <$> stripPrefix "GHC:finished: " markername)
          parseMarker _ = Nothing
          stripPrefix :: String -> String -> Maybe String
          stripPrefix prefix string =
              if prefix `isPrefixOf` string
              then Just $ drop (length prefix) string
              else Nothing

processEventLog :: EventLog -> ChromeTracing
processEventLog (EventLog (Header eventTypes) (Data events)) =
    ChromeTracing $ mapMaybe toChromeEvent events

processEventLogs :: [EventLog] -> ChromeTracing
processEventLogs logs = processEventLog $ foldr mergeEventLogs emptyEventLog logs
