module Sound.Tidal.Listener
  ( main
  ) where

import           Control.Concurrent
import           Control.Monad.State
import           Network.WebSockets
import           Sound.OSC.Time                as O
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

chatM :: String -> GameState ()
chatM s = gets sWS >>= liftIO . sendChatMessage s

getCps :: GameState Tempo.Tempo
getCps = gets sStream >>= liftIO . readMVar . T.sTempoMV

-- act on a message from twitch
act :: String -> TwitchPattern -> GameState ()
-- send code to tidal
act "!t" msg = do

  let code = content msg
      user = username msg

  gets sIn >>= \i -> liftIO $ putMVar i (drop 4 code)
  r <- gets sOut >>= liftIO . takeMVar

  case r of
    -- silently send pattern to tidal
    (HintOK pat) -> gets sStream >>= \s -> liftIO $ T.streamReplace s user pat
    -- complain to user about error in pattern
    (HintError e) -> gets sWS >>= liftIO . sendChatMessage text
      where text = user ++ " error in pattern: " ++ show e

-- query the cps
act "!cps" _ = do
  getCps >>= \cps -> chatM ("the cps is " ++ (show $ Tempo.cps cps))

-- set cps (right now this parses the result as a pattern, so stuff like
-- setcps(160/60/4) isn't possible, only setcps 0.5 etc)
act "!setcps" msg = do
  gets sStream >>= \s ->
    liftIO $ T.streamOnce s $ T.cps $ T.parseBP_E $ (drop 8 $ content msg)

-- get the current cycle
act "!now" _ = do
  cps <- getCps
  now <- O.time
  chatM
    $  "the current cycle is "
    ++ (show $ fromRational $ Tempo.timeToCycles cps now)


act _ _ = return ()
