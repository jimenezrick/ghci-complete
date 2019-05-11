module App where

import RIO
import RIO.Process
import System.Environment (lookupEnv)

import App.Class
import App.State
import Cache

data App = App
    { appLogOpts :: !LogOptions
    , appLogFunc :: !LogFunc
    , appProcessContext :: !ProcessContext
    , appReqChan :: !(MVar Request)
    , appState :: !(SomeRef AppState)
    }

instance HasLogOpts App where
    logOptsL = lens appLogOpts (\x y -> x {appLogOpts = y})

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

instance HasStateRef AppState App where
    stateRefL = lens appState (\x y -> x {appState = y})

runApp :: MonadIO m => RIO App a -> m a
runApp m =
    liftIO $ do
        verbose <- isJust <$> lookupEnv "GHCI_COMPLETE_VERBOSE"
        lo <- logOptionsHandle stderr verbose
        pc <- mkDefaultProcessContext
        rc <- newEmptyMVar
        st <- newSomeRef AppState {_appInfoCache = emptyCache}
        withLogFunc lo $ \lf ->
            let app =
                    App
                        { appLogOpts = lo
                        , appLogFunc = lf
                        , appProcessContext = pc
                        , appReqChan = rc
                        , appState = st
                        }
             in runRIO app m
