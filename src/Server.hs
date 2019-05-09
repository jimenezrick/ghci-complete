module Server where

import RIO
import RIO.ByteString (ByteString)
import RIO.Directory (removeFile)
import RIO.Partial

import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T

import Data.Aeson as A
import Data.Scientific (toBoundedInteger)
import Language.Haskell.Ghcid
import System.Environment (getArgs)
import System.Random (randomRIO)
import Text.Printf
import UnliftIO.Exception (onException)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Network.Simple.TCP as N
import qualified Network.Socket as N (getPeerName)

import App
import Complete
import Parse

-- TODO: send {"action": "reload"} on write?
--
-- https://github.com/dramforever/vscode-ghc-simple
runServer :: RIO App ()
runServer = do
    args <- liftIO getArgs
    let opts =
            case args of
                [] -> "cabal new-repl"
                files -> printf "ghci %s" $ unwords files
    port :: Int <- liftIO $ randomRIO (1024, 4096)
    let srvAddr = printf "localhost:%d" port
    writeAddressFile ".ghci_complete" srvAddr
    logInfo . fromString $ printf "Starting ghci-complete listening on %s" srvAddr
    withRunInIO
        (\runInIO ->
             N.serve
                 N.HostAny
                 (show port)
                 (\(sock, _addr) -> do
                      cliAddr <- liftIO $ N.getPeerName sock
                      runInIO . logDebug . fromString . printf "Client connected %s" $ show cliAddr
                      (ghci, _load) <-
                          startGhci opts Nothing $ \_stream msg ->
                              runInIO . logDebug $ fromString ("GHCi: " ++ msg)
                      runInIO $ serve sock ghci)) `onException`
        removeFile ".ghci_complete"
  where
    writeAddressFile path addr = B.writeFile path . encodeUtf8 . T.pack $ addr ++ "\n"

serve :: N.Socket -> Ghci -> RIO App ()
serve sock ghci = do
    line <- recv sock
    case line of
        Nothing -> do
            cliAddr <- liftIO $ N.getPeerName sock
            logDebug . fromString . printf "Client disconnected %s" $ show cliAddr
        Just line'
            --putStr "Command: "
            --B.putStr $ line' `B.append` "\n"
         -> do
            let msg =
                    case eitherDecodeStrict line' of
                        Left err -> error err
                        Right msg -> msg
                (Number id_, Object cmd) =
                    case msg of
                        Array array -> (array V.! 0, array V.! 1)
                        _ -> error "Fuck"
            let Just id' = toBoundedInteger id_ :: Maybe Int
            case H.lookup "command" cmd of
                Just (String "findstart") -> do
                    let String line = fromJust $ H.lookup "line" cmd
                        Number col = fromJust $ H.lookup "column" cmd
                        (_, start) = findStart line (fromJust $ toBoundedInteger col)
                    reply sock id' $ A.Object [("start", A.Number $ fromIntegral start)]
                Just (String "complete") -> do
                    let Number col = fromJust $ H.lookup "column" cmd
                        String line = fromJust $ H.lookup "line" cmd
                        Number first = fromJust $ H.lookup "complete_first" cmd
                        Number last = fromJust $ H.lookup "complete_last" cmd
                        (candidate, _) = findStart line (fromJust $ toBoundedInteger col)
                    let first' = fromJust $ toBoundedInteger first
                        last' = fromJust $ toBoundedInteger last
                    completion <- liftIO $ performCompletion ghci (Just (first', last')) candidate
                    case completion of
                        Just (results, more) -> do
                            let results' = Array . V.fromList $ map fmtCandidate results
                            reply sock id' $ A.Object [("results", results'), ("more", A.Bool more)]
                        Nothing -> error "Error: completion failed"
                Just (String "typeat") -> do
                    let String file = fromJust $ H.lookup "file" cmd
                        Number col = fromJust $ H.lookup "column" cmd
                        Number line = fromJust $ H.lookup "line" cmd
                        String under = fromJust $ H.lookup "under" cmd
                    let col' = fromJust $ toBoundedInteger col
                        line' = fromJust $ toBoundedInteger line
                    type_ <-
                        liftIO $
                        ghciTypeAt ghci (T.unpack file) line' col' (line' + 1) (col' + 1) under
                    case type_ of
                        Just type' ->
                            reply sock id' $
                            A.Object [("type", A.String type'), ("expr", A.String under)]
                        Nothing -> error "Error: type inference failed"
                Just (String "reload")
                    --threadDelay 10000000
                 -> do
                    liftIO $ ghciLoad ghci Nothing
                    reply sock id' A.Null
                Just (String "load") -> do
                    let String file = fromJust $ H.lookup "file" cmd
                    liftIO $ ghciLoad ghci (Just file)
                    reply sock id' A.Null
                _ -> error "Error: unknown received command"
    serve sock ghci
  where
    fmtCandidate (Candidate c t i) =
        A.Object [("word", String c), ("menu", String t), ("info", String i)]

-- XXX: Try to parse the JSON, if it fails, fetch more
recv :: (MonadIO m, MonadReader env m, HasLogFunc env) => N.Socket -> m (Maybe ByteString)
recv sock = N.recv sock (1024 * 1024)

reply :: (MonadIO m, MonadReader env m, HasLogFunc env) => N.Socket -> Int -> Value -> m ()
reply sock id' resp = do
    cliAddr <- liftIO $ N.getPeerName sock
    logDebug . fromString $ printf "Response %s: %s" (show cliAddr) (show resp)
    N.send sock . BL.toStrict . encode . Array $ V.fromList [Number $ fromIntegral id', resp]
