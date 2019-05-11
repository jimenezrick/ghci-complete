module Worker where

import RIO

import Language.Haskell.Ghcid

import App
import Complete
import Parse

data Request = Request
    { completion :: Completion
    , response :: MVar (Maybe Response)
    }

data Response = Response
    { match :: Candidate
    , more :: Bool
    }

startWorker :: (MonadUnliftIO m, MonadReader env m, HasLogOpts env) => String -> m (MVar Request)
startWorker cmd = do
    logDebug "Starting worker process"
    logOpts <- setLogUseLoc False <$> view logOptsL
    withLogFunc logOpts $ \logf ->
        local (set logFuncL logf) $ do
            (ghci, _load) <-
                withRunInIO
                    (\runInIO ->
                         startGhci cmd Nothing $ \_stream msg ->
                             runInIO . logDebug $ fromString ("GHCi: " ++ msg))
            mvar <- newEmptyMVar
            void . async $ workerLoop ghci mvar
            return mvar

--
-- TODO: Need to send multiple responses sometimes
--
workerLoop :: MonadIO m => Ghci -> MVar Request -> m ()
workerLoop ghci reqsChan = do
    req <- takeMVar reqsChan
    resp <- handleCompletion $ completion req
    putMVar (response req) resp
    workerLoop ghci reqsChan

sendRequest :: MonadIO m => Completion -> m (MVar (Maybe Response))
sendRequest = undefined

recvResponse :: MonadIO m => MVar (Maybe Response) -> m (Maybe Response)
recvResponse = undefined

handleCompletion :: MonadIO m => Completion -> m (Maybe Response)
handleCompletion = undefined
