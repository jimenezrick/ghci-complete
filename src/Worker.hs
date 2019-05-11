module Worker where

import RIO

import Language.Haskell.Ghcid

import App.Class
import App.State
import Complete
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
--       Also do the cachign
--
workerLoop :: MonadIO m => Ghci -> MVar Request -> m ()
workerLoop ghci reqChan = do
    req <- takeMVar reqChan
    matches <- performCompletion ghci (req ^. range) (req ^. completion)
    case matches of
        Nothing -> putMVar (req ^. respChan) Nothing
        Just (matches, more) ->
            putMVar (req ^. respChan) $ Just Response {_matches = matches, _more = more}
    workerLoop ghci reqChan

-- TODO: return list with all the completion
performAsyncCompletion ::
       MonadIO m => MVar Request -> Maybe Range -> Completion -> m (Maybe ([Match], Bool))
performAsyncCompletion reqChan range compl = do
    respChan <- newEmptyMVar
    putMVar reqChan Request {_range = range, _completion = compl, _respChan = respChan}
    resp <- takeMVar respChan
    case resp of
        Nothing -> return Nothing
        Just (Response matches more) -> return $ Just (matches, more)
