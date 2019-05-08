module App where

import RIO
import RIO.Process
import System.Environment (lookupEnv)

import Cache

data App = App
    { appLogFunc :: !LogFunc
    , appProcessContext :: !ProcessContext
    , appInfoCache :: !(SomeRef InfoCache)
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

instance HasStateRef InfoCache App where
    stateRefL = lens appInfoCache (\x y -> x {appInfoCache = y})

runApp :: MonadIO m => RIO App a -> m a
runApp m =
    liftIO $ do
        verbose <- isJust <$> lookupEnv "GHCI_COMPLETE_VERBOSE"
        lo <- logOptionsHandle stderr verbose
        pc <- mkDefaultProcessContext
        ic <- newSomeRef empty
        withLogFunc lo $ \lf ->
            let app = App {appLogFunc = lf, appProcessContext = pc, appInfoCache = ic}
             in runRIO app m
