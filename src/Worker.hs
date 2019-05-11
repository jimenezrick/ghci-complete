module Worker where

import RIO

import Language.Haskell.Ghcid

import App.Class
import App.State
import Parse

startWorker ::
       (MonadUnliftIO m, MonadReader env m, HasLogOpts env) => String -> MVar Request -> m ()
startWorker cmd reqChan = do
    logDebug "Starting worker process"
    logOpts <- setLogUseLoc False <$> view logOptsL
    withLogFunc logOpts $ \logf ->
        local (set logFuncL logf) $ do
            (ghci, _load) <-
                withRunInIO
                    (\runInIO ->
                         startGhci cmd Nothing $ \_stream msg ->
                             runInIO . logDebug $ fromString ("GHCi: " ++ msg))
            void . async $ workerLoop ghci reqChan

--
-- TODO: Need to send multiple responses sometimes
--
workerLoop :: MonadIO m => Ghci -> MVar Request -> m ()
workerLoop ghci reqChan = do
    req <- takeMVar reqChan
    resp <- handleCompletion $ completion req
    putMVar (response req) resp
    workerLoop ghci reqChan

sendRequest :: MonadIO m => Completion -> m (MVar (Maybe Response))
sendRequest = undefined

recvResponse :: MonadIO m => MVar (Maybe Response) -> m (Maybe Response)
recvResponse = undefined

handleCompletion :: MonadIO m => Completion -> m (Maybe Response)
handleCompletion = undefined
