module Sound.Tidal.Data where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad.State
import           Network.WebSockets
import qualified Sound.Tidal.Context           as T
import           Sound.Tidal.Hint

data GS = GS
  { sIn             :: MVar String
  , sOut            :: MVar Sound.Tidal.Hint.Response
  , sStream         :: T.Stream
  , sWS             :: Connection
  , sActivePatterns :: [TwitchPattern]
  , sMaxPat         :: Integer
  }

data TwitchPattern = TwitchPattern
  { username :: String
  , userid   :: String -- TODO int
  , content  :: String
  }
  deriving Show

type GameState a = StateT GS IO a
