module Sound.Tidal.Listener
  ( main
  ) where

import           Control.Concurrent
import           Control.Monad.State
import           Network.WebSockets
import           Sound.OSC.FD                  as O
import qualified Sound.Tidal.Context           as T
import           Sound.Tidal.Hint
import           Sound.Tidal.Stream             ( Target(..) )
import qualified Sound.Tidal.Tempo             as Tempo
import           Wuss

import           Sound.Tidal.Data
import           Sound.Tidal.Twitch

main :: IO ()
main = do
  putStrLn "connecting to twitch..."
  runSecureClient "irc-ws.chat.twitch.tv" 443 "/" ws

startHint :: IO (MVar String, MVar Sound.Tidal.Hint.Response)
startHint = do
  mIn  <- newEmptyMVar
  mOut <- newEmptyMVar
  _    <- forkIO $ hintJob mIn mOut
  return (mIn, mOut)

ws :: ClientApp ()
ws twitch = do

  initialise twitch
  (mIn, mOut) <- startHint
  putStrLn "starting tidal..."
  stream <- T.startStream
    T.defaultConfig
    [(T.superdirtTarget { oLatency = 0.1 }, [T.superdirtShape])]

  -- init state
  let gs = GS mIn mOut stream twitch []

  forever $ runStateT run gs >> return ()

run :: GameState ()
run = do
  liftIO $ putStrLn "waiting..."
  -- get latest message
  latest <- gets sWS >>= liftIO . receiveData

  -- act on latest
  case parse latest of
    Just p -> act (getCmd p) p
    _      -> return ()

getCmd :: TwitchPattern -> String
getCmd = takeWhile (/= ' ') . content

-- act on a message from twitch
act :: String -> TwitchPattern -> GameState ()
-- send code to tidal
act "!t" msg = do

  let code = content msg
      user = username msg

  gets sIn >>= \i -> liftIO $ putMVar i (drop 4 code)
  response <- gets sOut >>= liftIO . takeMVar

  case response of
    -- silently send pattern to tidal
    (HintOK pat) -> gets sStream >>= \s -> liftIO $ T.streamReplace s user pat
    -- complain to user about error in pattern
    (HintError e) -> gets sWS >>= liftIO . sendChatMessage text
      where text = user ++ " error in pattern: " ++ show e

-- query the cps
act "!cps" _ = do
  cps <- gets sStream >>= liftIO . readMVar . T.sTempoMV
  let msg = "the cps is " ++ (show $ Tempo.cps cps)
  gets sWS >>= liftIO . sendChatMessage msg

act _ _ = return ()
