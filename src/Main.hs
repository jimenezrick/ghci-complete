import Debug.Trace

import Control.Monad (forM)
import Data.Aeson as A
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.Either
import Data.Maybe
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Text.Printf

import GHC.SyntaxHighlighter
import Language.Haskell.Ghcid

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Network.Simple.TCP as N

-- TODO: send {"action": "reload"} on write?

-- :type-at :set +c
-- :loc-at
-- expand('<cWORD>')
--
-- reload code command
--
-- https://github.com/dramforever/vscode-ghc-simple

main :: IO ()
main = do
    putStrLn "Running server..."
    writeAddressFile ".ghci_complete" "8000"
    N.serve
        N.HostAny
        "8000" -- XXX: make it random
        (\(sock, _addr) -> do
             putStrLn "Client connected"
             (ghci, _load) <- startGhci "cabal new-repl" Nothing printOutput
             serve sock ghci)
  where
    printOutput _stream text = putStrLn text

writeAddressFile :: FilePath -> N.ServiceName -> IO ()
writeAddressFile path port = do
    writeFile path $ printf "localhost:%s\n" port

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
            B.putStrLn line'
            let msg =
                    case eitherDecodeStrict line' of
                        Left err -> error err
                        Right msg -> msg
                (Number id_, Object cmd) =
                    case msg of
                        Array array -> (array V.! 0, array V.! 1)
                        _ -> error "Fuck"
            let Just id' = (toBoundedInteger id_) :: Maybe Int
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
                    (results, more) <-
                        performCompletion
                            ghci
                            (Just (fromJust $ toBoundedInteger first, fromJust $ toBoundedInteger last))
                            candidate
                    let results' = Array . V.fromList $ map fmtCandidate results
                    reply sock id' $ A.Object [("results", results'), ("more", A.Bool more)]
                _ -> error "Error: unknown received command"
            serve sock ghci
  where
    fmtCandidate (Candidate c t i) = A.Object [("word", String c), ("menu", String t), ("info", String i)]

ghciType :: Ghci -> Text -> IO Text
ghciType ghci expr
    | Just [(OperatorTok, op)] <- tokenizeHaskell expr =
        T.unwords . map T.stripStart <$> evalRpl ghci (printf ":type (%s)" op)
    | otherwise = T.unwords . map T.stripStart <$> evalRpl ghci (printf ":type %s" expr)

ghciInfo :: Ghci -> Text -> IO [Text]
ghciInfo ghci expr
    | Just [(OperatorTok, op)] <- tokenizeHaskell expr = evalRpl ghci $ printf ":info (%s)" op
    | otherwise = evalRpl ghci $ printf ":info %s" expr

ghciBrowse :: Ghci -> Text -> IO [Text]
ghciBrowse ghci mod = evalRpl ghci $ printf ":browse %s" mod

ghciComplete :: Ghci -> Maybe (Int, Int) -> Completion -> IO ([Text], Bool)
ghciComplete ghci range compl = do
    candidates <- evalRpl ghci $ cmd range
    let [num, total] = map parseDigit $ take 2 $ T.words $ head candidates
    return (map (T.init . T.tail) $ tail candidates, more total range)
  where
    prefix (Module mod _) = printf "import %s" (T.unpack mod)
    prefix (ModuleExport mod var _) = printf "%s.%s" (T.unpack mod) (T.unpack var) -- FIXME: remove module from results
    prefix (Variable var _) = T.unpack var
    cmd Nothing = printf ":complete repl \"%s\"" $ prefix compl
    cmd (Just (first,  last)) = printf ":complete repl %d-%d \"%s\"" first last $ prefix compl
    parseDigit = fst . fromRight (-1, "") . T.decimal
    more total (Just (first, last)) = last < total
    more _ Nothing = False

data Candidate = Candidate
    { candidate :: Text
    , type_ :: Text
    , info :: Text
    }

performCompletion :: Ghci -> Maybe (Int, Int) -> Completion -> IO ([Candidate], Bool)
performCompletion ghci range compl = do
    (candidates, more) <- ghciComplete ghci range compl
    candidates' <- forM candidates $ \c -> do
        t <- T.dropWhile isSpace . T.dropWhile (not . isSpace) <$> ghciType ghci c
        i <- T.unlines <$> case compl of
                               (Module mod _) -> ghciBrowse ghci c
                               (ModuleExport mod var _) -> ghciInfo ghci c -- XXX: build prefix?
                               (Variable var _) -> ghciInfo ghci c
        return $ Candidate c t i
    return (candidates', more)

evalRpl :: Ghci -> String -> IO [Text]
evalRpl ghci cmd = map T.pack <$> exec ghci cmd

findStart :: Text -> Int -> (Completion, Int)
findStart line col =
    let (start, _) = T.splitAt (col - 1) line -- Column is [1..N] and column=X means text in [1..X-1]
     in trace (printf "debug: findStart \"%s\" %d" line col) $
        traceShowId $
        case parseCompletion start of
            Nothing -> (Variable "" (Loc 1 col 1 col), col)
            Just mod@(Module _ loc) -> (mod, startCol loc)
            Just var@(Variable _ loc) -> (var, startCol loc)
  where
    startCol (Loc _ c _ _) = c - 1 -- The text starts after the index X we return, so [X+1..]

data Completion = Module Text Loc
                | ModuleExport Text Text Loc
                | Variable Text Loc
                deriving Show

-- TODO: ModuleExport
decideCompletion :: [(Token, Text, Loc)] -> Maybe Completion
decideCompletion [] = Just $ Variable "" (Loc 0 1 0 0)
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
    | [(VariableTok, var, loc)] <- takeLast 1 tokens = Just $ Variable var loc
    | otherwise = error "decideCompletion: missing completion case"
  where
    takeLast n = reverse . take n . reverse

parseCompletion :: Text -> Maybe Completion
parseCompletion line =
    case (dropSpaces <$> tokenizeHaskell line, tokenizeHaskellLoc line) of
        (Just tokens, Just locs) ->
            let tokens' = zip3 (map fst tokens) (map snd tokens) (map snd locs)
             in decideCompletion tokens'
        _ -> Nothing
  where
    dropSpaces = filter ((SpaceTok /=) . fst)
