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
  let gs = GS mIn mOut stream twitch [] 4

  runStateT loop gs >> return ()

loop :: GameState ()
loop = do
  liftIO $ putStrLn "waiting..."
  -- get latest message
  latest <- gets sWS >>= liftIO . receiveData

  -- act on latest
  case parse latest of
    Just p -> act (getCmd p) p
    _      -> return ()

  loop

getCmd :: TwitchPattern -> String
getCmd = takeWhile (/= ' ') . content

chatM :: String -> GameState ()
chatM s = gets sWS >>= liftIO . sendChatMessage s

getCps :: GameState Tempo.Tempo
getCps = gets sStream >>= liftIO . readMVar . T.sTempoMV

sandbox :: TwitchPattern -> GameState ()
sandbox latest = do
  -- maxpat <- gets sMaxPat
  -- active <- gets sActivePatterns
  -- if length active == 0
  --   then
  return ()

-- pop a specific users pattern off the active patterns
pop :: String -> GameState ()
pop user = get >>= \st -> do
  liftIO $ T.streamMute (sStream st) user
  put $ GS (sIn st)
           (sOut st)
           (sStream st)
           (sWS st)
           (filter (\p -> user /= username p) $ sActivePatterns st)
           (sMaxPat st)

-- push a new pattern onto the active patterns
push :: TwitchPattern -> GameState ()
push msg = get >>= \st -> put $ GS (sIn st)
                                   (sOut st)
                                   (sStream st)
                                   (sWS st)
                                   (sActivePatterns st ++ [msg])
                                   (sMaxPat st)


-- act on a message from twitch
act :: String -> TwitchPattern -> GameState ()

-- list active patterns
act "!l"    _   = gets sActivePatterns >>= \ps -> chatM (show ps)

-- remove a users message from the active patterns & hush it
act "!pop"  msg = pop (drop 5 $ content msg)

-- hush everything
act "!hush" _   = gets sStream >>= liftIO . T.streamHush

-- send code to tidal
act "!t"    msg = do

  let code = content msg
      user = username msg

  gets sIn >>= \i -> liftIO $ putMVar i (drop 3 code)
  r <- gets sOut >>= liftIO . takeMVar

  case r of
    -- silently send pattern to tidal, and add it to active pattern stack
    (HintOK pat) -> do
      gets sStream >>= \s -> liftIO $ T.streamReplace s user pat
      push msg
    -- complain to user about error in pattern
    (HintError e) -> gets sWS >>= liftIO . sendChatMessage text
      where text = user ++ " error in pattern: " ++ show e

-- query the cps
act "!cps" _ =
  getCps >>= \cps -> chatM ("the cps is " ++ (show $ Tempo.cps cps))

-- set cps (right now this parses the result as a pattern, so stuff like
-- setcps(160/60/4) isn't possible, only setcps 0.5 etc)
act "!setcps" msg = gets sStream >>= \s ->
  liftIO $ T.streamOnce s $ T.cps $ T.parseBP_E $ (drop 8 $ content msg)

-- get the current cycle
act "!now" _ = do
  cps <- getCps
  now <- O.time
  chatM
    $  "the current cycle is "
    ++ (show $ fromRational $ Tempo.timeToCycles cps now)


act _ _ = return ()
