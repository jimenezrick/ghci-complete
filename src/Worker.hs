module Worker where

import RIO

import Language.Haskell.Ghcid

data Request

data Response = Response
    { more :: Bool
    , match :: Match
    }

startWorker :: (MonadIO m) => m (Ghci, MVar Request)
startWorker = undefined

sendRequest = undefined
