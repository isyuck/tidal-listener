{-# LANGUAGE OverloadedStrings #-}
module Sound.Tidal.Twitch where

import           Control.Monad                  ( liftM2 )
import qualified Data.Text                     as T
import           Network.WebSockets

import           Sound.Tidal.Data

channel = "isyuck"
botuser = "kddzi"
oauth = "oauth:00mthxmadwwnm5eqpxoimwbcijngn4"

initialise :: Connection -> IO ()
initialise c = do
  mapM_
    (sendTextData c . T.pack)
    [ "PASS " ++ oauth
    , "NICK " ++ botuser
    , "JOIN #" ++ channel
    , "CAP REQ :twitch.tv/tags"
    ]
  sendChatMessage "hs-party running..." c

filterTags :: T.Text -> Maybe [T.Text]
filterTags tags | (length ft >= 3) = Just ft
                | otherwise        = Nothing
 where
  ft =
    map (T.stripStart . T.drop 1 . T.dropWhile (/= '=') . snd)
      $ filter (\(w, t) -> T.isInfixOf w t)
      $ liftM2 (,) wanted
      $ T.split (== ';') tags
  wanted = ["user-type", "display-name", "user-id"]

-- turn the raw received message from the twitch api into a TwitchMessage
-- NOTE that this depends on the order that twitch sends its return messages
parse :: T.Text -> Maybe TwitchPattern
parse t = case filterTags t of
  Nothing       -> Nothing
  Just filtered -> Just $ TwitchPattern un uid msg
   where
    msg =
      T.unpack $ T.dropEnd 2 $ T.drop 2 $ snd $ T.breakOn " :" $ filtered !! 0
    un  = T.unpack $ filtered !! 1
    uid = T.unpack $ filtered !! 2

sendChatMessage :: String -> Connection -> IO ()
sendChatMessage m c = do
  sendTextData c $ T.pack ("PRIVMSG #" ++ channel ++ " :" ++ m)
