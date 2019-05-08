module Complete where

import RIO
import RIO.ByteString (ByteString)
import RIO.Text (Text)

import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T

-- FIXME: use RIO and logging
import Prelude (head, putStr, putStrLn, tail, zip3)

import Control.Monad (forM, void)
import Data.Aeson as A
import Data.Char
import Data.List (find)
import Data.Maybe
import Data.Scientific (toBoundedInteger)
import System.Random (randomRIO)
import Text.Printf

import GHC.SyntaxHighlighter
import Language.Haskell.Extension (KnownExtension)
import Language.Haskell.Ghcid
import System.Environment (getArgs)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Network.Simple.TCP as N

import App
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
    let addr = printf "localhost:%d" port
    writeAddressFile ".ghci_complete" addr
    logInfo . fromString $ printf "Starting ghci-complete listening on: %s" addr
    N.serve
        N.HostAny
        (show port)
        (\(sock, _addr) -> do
             putStrLn "Client connected"
             (ghci, _load) <- startGhci opts Nothing printGhciOutput
             serve sock ghci)
  where
    printGhciOutput _stream = putStrLn
    writeAddressFile path addr = liftIO . B.writeFile path . encodeUtf8 . T.pack $ addr ++ "\n"

-- XXX: Try to parse the JSON, if it fails, fetch more
recv :: N.Socket -> IO (Maybe ByteString)
recv sock = N.recv sock (1024 * 1024)

reply :: N.Socket -> Int -> Value -> IO ()
reply sock id' resp = do
    putStrLn "reply"
    N.send sock . BL.toStrict . encode . Array $ V.fromList [Number $ fromIntegral id', resp]

serve :: N.Socket -> Ghci -> IO ()
serve sock ghci = do
    line <- recv sock
    case line of
        Nothing -> putStrLn "Connection closed"
        Just line' -> do
            putStr "Command: "
            B.putStr $ line' `B.append` "\n"
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
                    completion <- performCompletion ghci (Just (first', last')) candidate
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
                    type_ <- ghciTypeAt ghci (T.unpack file) line' col' (line' + 1) (col' + 1) under
                    case type_ of
                        Just type' ->
                            reply sock id' $
                            A.Object [("type", A.String type'), ("expr", A.String under)]
                        Nothing -> error "Error: type inference failed"
                Just (String "reload")
                    --threadDelay 10000000
                 -> do
                    ghciLoad ghci Nothing
                    reply sock id' A.Null
                Just (String "load") -> do
                    let String file = fromJust $ H.lookup "file" cmd
                    ghciLoad ghci (Just file)
                    reply sock id' A.Null
                _ -> error "Error: unknown received command"
            serve sock ghci
  where
    fmtCandidate (Candidate c t i) =
        A.Object [("word", String c), ("menu", String t), ("info", String i)]

ghciLoad :: Ghci -> Maybe Text -> IO ()
ghciLoad ghci (Just path) = void $ evalExpr ghci $ printf ":load! %s" path
ghciLoad ghci Nothing = void $ evalExpr ghci ":reload!"

-- TODO: tokenize line and find right expression
ghciTypeAt :: Ghci -> FilePath -> Int -> Int -> Int -> Int -> Text -> IO (Maybe Text)
ghciTypeAt ghci file line col line' col' expr
    | Just [(OperatorTok, op)] <- tokenizeHaskell expr =
        fmap joinLines <$>
        evalExpr ghci (printf ":type-at %s %d %d %d %d (%s)" file line col line' col' op)
    | otherwise =
        fmap joinLines <$>
        evalExpr ghci (printf ":type-at %s %d %d %d %d %s" file line col line' col' expr)
  where
    joinLines = T.unwords . map T.stripStart

ghciType :: Ghci -> Text -> IO (Maybe Text)
ghciType ghci expr
    | Just [(OperatorTok, op)] <- tokenizeHaskell expr =
        fmap joinLines <$> evalExpr ghci (printf ":type (%s)" op)
    | otherwise = fmap joinLines <$> evalExpr ghci (printf ":type %s" expr)
  where
    joinLines = T.unwords . map T.stripStart

ghciInfo :: Ghci -> Text -> IO (Maybe [Text])
ghciInfo ghci expr
    | Just [(OperatorTok, op)] <- tokenizeHaskell expr = evalExpr ghci $ printf ":info (%s)" op
    | otherwise = evalExpr ghci $ printf ":info %s" expr

ghciBrowse :: Ghci -> Text -> IO (Maybe [Text])
ghciBrowse ghci mod = evalExpr ghci $ printf ":browse! %s" mod

ghciComplete :: Ghci -> Maybe (Int, Int) -> Completion -> IO (Maybe ([Text], Bool))
ghciComplete ghci range compl = do
    candidates <- evalExpr ghci $ cmd range
    return $ do
        cs <- candidates
        let [_num, total] = map parseDigit $ take 2 $ T.words $ head cs
        return (map (T.init . T.tail) $ tail cs, more total range)
  where
    prefix (Module mod _) = printf "import %s" (T.unpack mod)
    prefix (ModuleExport mod var _) = printf "%s.%s" (T.unpack mod) (T.unpack var) -- FIXME: remove module from results
    prefix (Variable var _) = T.unpack var
    cmd Nothing = printf ":complete repl \"%s\"" $ prefix compl
    cmd (Just (first, last)) = printf ":complete repl %d-%d \"%s\"" first last $ prefix compl
    parseDigit :: Text -> Int
    parseDigit = fromJust . readMaybe . T.unpack
    more total (Just (first, last)) = last < total
    more _ Nothing = False

performCompletion :: Ghci -> Maybe (Int, Int) -> Completion -> IO (Maybe ([Candidate], Bool))
performCompletion _ _ (Extension ext _) =
    let extensions = filter (T.isPrefixOf ext) ghcExtensions
     in return $ Just (map (\e -> Candidate e "" "") extensions, False)
performCompletion ghci range compl = do
    completion <- ghciComplete ghci range compl
    case completion of
        Just (candidates, more) -> do
            candidates' <-
                forM candidates $ \c
                    -- This can fail when using :info on a type
                 -> do
                    t <-
                        maybe "" (T.dropWhile isSpace . T.dropWhile (not . isSpace)) <$>
                        ghciType ghci c
                    i <-
                        fmap T.unlines <$>
                        case compl of
                            (Module _ _) -> ghciBrowse ghci c
                            (ModuleExport _ _ _) -> ghciInfo ghci c -- XXX: build prefix?
                            (Variable _ _) -> ghciInfo ghci c
                            (Extension _ _) -> error "performCompletion: impossible"
                    return $ Candidate c t (fromMaybe (error "performCompletion: ghci failed") i)
            return $ Just (candidates', more)
        Nothing -> return Nothing

evalExpr :: Ghci -> String -> IO (Maybe [Text])
evalExpr ghci cmd = do
    out <- exec ghci cmd
    case map words out of
        []:("<interactive>:1:1:":"error:":_):_ -> return Nothing
        _ -> return . Just $ map T.pack out

findStart :: Text -> Int -> (Completion, Int)
findStart line col =
    let (start, _) = T.splitAt (col - 1) line -- Column is [1..N] and column=X means text in [1..X-1]
     in case parseCompletion start of
            Nothing -> (Variable "" (Loc 1 col 1 col), col)
            Just mod@(Module _ loc) -> (mod, startCol loc)
            Just var@(Variable _ loc) -> (var, startCol loc)
            Just ext@(Extension _ loc) -> (ext, startCol loc)
  where
    startCol (Loc _ c _ _) = c - 1 -- The text starts after the index X we return, so [X+1..]

decideCompletion :: [(Token, Text, Loc)] -> Maybe Completion
decideCompletion [] = Just $ Variable "" (Loc 0 1 0 0)
decideCompletion [(KeywordTok, "import", loc)] = Just $ Module "" loc
decideCompletion [(KeywordTok, "import", _), (KeywordTok, "qualified", loc)] = Just $ Module "" loc
decideCompletion tokens@((KeywordTok, "import", _):(KeywordTok, "qualified", _):(ConstructorTok, mod, _):_)
    | Just compl <- completeModuleExport mod tokens = Just compl
decideCompletion tokens@((KeywordTok, "import", _):(ConstructorTok, mod, _):_)
    | Just compl <- completeModuleExport mod tokens = Just compl
decideCompletion ((KeywordTok, "import", _):(ConstructorTok, mod, loc):(OperatorTok, ".", _):_) =
    Just $ Module (mod `T.append` ".") loc
decideCompletion ((KeywordTok, "import", _):(ConstructorTok, mod, loc):_) = Just $ Module mod loc
decideCompletion ((KeywordTok, "import", _):(KeywordTok, "qualified", _):(ConstructorTok, mod, loc):(OperatorTok, ".", _):_) =
    Just $ Module (mod `T.append` ".") loc
decideCompletion ((KeywordTok, "import", _):(KeywordTok, "qualified", _):(ConstructorTok, mod, loc):_) =
    Just $ Module mod loc
decideCompletion tokens
    | [(ConstructorTok, var, loc), (OperatorTok, ".", _)] <- takeLast 2 tokens =
        Just $ Variable (var `T.append` ".") loc
    | [(ConstructorTok, var, loc)] <- takeLast 1 tokens = Just $ Variable var loc
    | [(OperatorTok, op, loc)] <- takeLast 1 tokens = Just $ Variable op loc
    | [(VariableTok, var, loc)] <- takeLast 1 tokens = Just $ Variable var loc
    | otherwise = error $ printf "decideCompletion: missing case: %s" $ show tokens
  where
    takeLast n = reverse . take n . reverse

completeModuleExport :: Text -> [(Token, Text, Loc)] -> Maybe Completion
completeModuleExport mod tokens
    | Just _ <-
         find
             (\case
                  (SymbolTok, "(", _) -> True
                  _ -> False)
             tokens =
        case reverse tokens of
            ((SymbolTok, "(", loc):_) -> Just $ ModuleExport mod "" loc
            ((SymbolTok, ",", loc):_) -> Just $ ModuleExport mod "" loc
            ((VariableTok, var, loc):_) -> Just $ ModuleExport mod var loc
            ((OperatorTok, op, loc):_) -> Just $ ModuleExport mod op loc
            _ -> error $ printf "completeModuleExport: missing case: %s %s" mod $ show tokens
    | otherwise = Nothing

parseCompletion :: Text -> Maybe Completion
parseCompletion line =
    case (filter ((SpaceTok /=) . fst) <$> tokenizeHaskell line, tokenizeHaskellLoc line) of
        (Just tokens, Just locs) ->
            let tokens' = zip3 (map fst tokens) (map snd tokens) (map snd locs)
             in decideCompletion tokens'
        _ ->
            case tokenizeWords line of
                [("{-#", _), ("LANGUAGE", _)] -> Just . Extension "" . locN $ T.length line + 1
                ("{-#", _):("LANGUAGE", _):(pre, loc):_ -> Just $ Extension pre loc
                _ -> Nothing
  where
    locN n = Loc 1 n 1 1

tokenizeWords :: Text -> [(Text, Loc)]
tokenizeWords line = map toToken . wordsCols $ zip (T.unpack line) [1 ..]
  where
    toToken cs = (T.pack $ map fst cs, Loc 1 (snd $ head cs) 1 1)
    wordsCols s =
        case dropWhile (isSpace . fst) s of
            [] -> []
            s' -> w : wordsCols s''
                where (w, s'') = break (isSpace . fst) s'

ghcExtensions :: [Text]
ghcExtensions = T.pack . show <$> ([minBound .. maxBound] :: [KnownExtension])
