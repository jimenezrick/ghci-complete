module App.State where

import RIO

import Lens.Micro.TH

import Cache
import Parse

data AppState = AppState
    { _appInfoCache :: InfoCache
    , _none :: ()
    }

makeLenses ''AppState

data Request = Request
    { completion :: Completion
    , response :: MVar (Maybe Response)
    }

data Response = Response
    { match :: Candidate
    , more :: Bool
    }
