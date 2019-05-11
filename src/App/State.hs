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

data Response = Response
    { _matches :: [Match]
    , _more :: Bool
    }

makeLenses ''Response

type Range = (Int, Int)

data Request = Request
    { _range :: Maybe Range
    , _completion :: Completion
    , _respChan :: MVar (Maybe Response)
    }

makeLenses ''Request
