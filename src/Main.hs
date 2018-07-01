{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Prelude hiding (lookup, hGetLine)

import Debug.Trace

import Control.Applicative
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, void)
import Data.Aeson as A
import Data.ByteString.Char8 as B
import Data.Char
import Data.Either
import Data.HashMap.Strict
import Data.Maybe
import Data.Scientific (toBoundedInteger)
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Read as T
import System.IO hiding (hGetLine)
import Text.Printf

import GHC.SyntaxHighlighter

import System.Posix.Files (createNamedPipe)
import Language.Haskell.Ghcid

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Simple.TCP as T
import qualified Data.Vector as V
import qualified Network.Socket as S

-- TODO: send {"action": "reload"} on write?

main :: IO ()
main = do
    Prelude.putStrLn "Running server..."
    writeAddressFile ".ghci_complete" "8000"
    T.serve
        T.HostAny
        "8000" -- XXX: make it random
        (\(sock, _addr) -> do
             Prelude.putStrLn "Client connected"
             (ghci, load) <- startGhci "cabal new-repl" Nothing printOutput
             serve sock ghci)
  where
    printOutput stream text = Prelude.putStrLn text

writeAddressFile :: FilePath -> T.ServiceName -> IO ()
writeAddressFile path port = do
    Prelude.writeFile path $ printf "localhost:%s\n" port

recv :: T.Socket -> IO (Maybe ByteString)
recv sock = T.recv sock (1024 * 1024)

reply :: T.Socket -> Int -> Value -> IO ()
reply sock id' resp = do
    Prelude.putStrLn "reply"
    T.send sock . BL.toStrict . encode . Array $ V.fromList [Number $ fromIntegral id', resp]

serve :: T.Socket -> Ghci -> IO ()
serve sock ghci = do
    line <- recv sock
    case line of
        Nothing ->
            Prelude.putStrLn "Connection closed"
        Just line' -> do
            Prelude.putStr "Command: "
            B.putStrLn line'

            let msg = case eitherDecodeStrict line' of
                        Left err -> error err
                        Right msg -> msg

                (Number id_, Object cmd) = case msg of
                                     Array array -> (array V.! 0, array V.! 1)
                                     _ -> error "Fuck"

            let Just id' = (toBoundedInteger id_) :: Maybe Int
            case lookup "command" cmd of
                Just (String "findstart") -> do
                    let String line  = fromJust $ lookup "line" cmd
                        Number col  = fromJust $ lookup "column" cmd

                        (_, start) = findStart line (fromJust $ toBoundedInteger col)

                    reply sock id' $ A.Object [("start", A.Number $ fromIntegral start)]

                Just (String "complete") -> do
                    let Number col  = fromJust $ lookup "column" cmd
                        String line  = fromJust $ lookup "line" cmd
                        Number first = fromJust $ lookup "complete_first" cmd
                        Number last = fromJust $ lookup "complete_last" cmd

                        (candidate, _) = findStart line (fromJust $ toBoundedInteger col)

                    (results, more) <- performCompletion ghci (Just (fromJust $ toBoundedInteger first, fromJust $ toBoundedInteger last)) candidate
                    let results' = Array . V.fromList $ Prelude.map fmtCandidate results
                    reply sock id' $ A.Object [("results", results'), ("more", A.Bool more)]

                _ -> error "Error: unknown received command"
            serve sock ghci

  where fmtCandidate (Candidate c  t  i) = A.Object [("word", String c), ("menu", String t), ("info", String i)]

ghciType :: Ghci -> Text -> IO Text
ghciType ghci expr = do
    Prelude.head <$> evalRpl ghci cmd
  where cmd = printf ":type %s" expr

ghciInfo :: Ghci -> Text -> IO [Text]
ghciInfo ghci expr = do
    evalRpl ghci cmd
  where cmd = printf ":info %s" expr

ghciBrowse :: Ghci -> Text -> IO [Text]
ghciBrowse ghci expr = do
    evalRpl ghci cmd
  where cmd = printf ":browse %s" expr

ghciComplete :: Ghci -> Maybe (Int, Int) -> Completion -> IO ([Text], Bool)
ghciComplete ghci range compl = do
    candidates <- evalRpl ghci $ cmd range
    let [num, total] = Prelude.map parseDigit $ Prelude.take 2 $ T.words $ Prelude.head candidates
    return (Prelude.map (T.init . T.tail) $ Prelude.tail candidates, more total range)
  where
    prefix (Module mod _) = printf "import %s" (T.unpack mod)
    prefix (ModuleExport mod var _) = printf "%s.%s" (T.unpack mod) (T.unpack var) -- FIXME: remove module from results
    prefix (Variable var _) = T.unpack var
    cmd Nothing = printf ":complete repl \"%s\"" $ prefix compl
    cmd (Just (first,  last)) = printf ":complete repl %d-%d \"%s\"" first last $ prefix compl
    parseDigit = fst . fromRight (-1, "") . decimal
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
evalRpl ghci cmd = Prelude.map T.pack <$> exec ghci cmd

findStart :: Text -> Int -> (Completion, Int)
findStart line col =
    let (start, _) = T.splitAt (col - 1) line -- Column is [1..N] and column=X means text in [1..X-1]
     in trace (printf "debug: findStart \"%s\" %d" line col) $
        traceShowId $
        case parseCompletion start of
            Nothing -> (Variable "" (Loc 1 col 1 col), col) -- XXX?
            Just mod@(Module _ loc) -> (mod, startCol loc)
            Just var@(Variable _ loc) -> (var, startCol loc)
  where
    startCol (Loc _ c _ _) = c - 1 -- The text starts after the index X we return, so [X+1..]

data Completion = Module Text Loc
                | ModuleExport Text Text Loc
                | Variable Text Loc
                deriving Show

decideCompletion :: [(Token, Text, Loc)] -> Maybe Completion
decideCompletion [] = Just $ Variable "" (Loc 0 1 0 0)
decideCompletion ((KeywordTok, "import", _):(ConstructorTok, mod, loc):_) = Just $ Module mod loc
decideCompletion (token@(_, var, loc):_)
    | (ConstructorTok, _, _) <- token = Just $ Variable var loc
    | (OperatorTok, _, _) <- token = Just $ Variable var loc
    | (VariableTok, _, _) <- token = Just $ Variable var loc
decideCompletion _ = error "decideCompletion: missing completion case"

parseCompletion :: Text -> Maybe Completion
parseCompletion line =
    case (dropSpaces <$> tokenizeHaskell line, tokenizeHaskellLoc line) of
        (Just tokens, Just locs) ->
            let tokens' = zip3 (Prelude.map fst tokens) (Prelude.map snd tokens) (Prelude.map snd locs)
             in decideCompletion tokens'
        _ -> Nothing
  where
    dropSpaces = Prelude.filter ((SpaceTok /=) . fst)

-- :type-at :set +c
-- :loc-at
-- expand('<cWORD>')
--
--
--
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:complete
--
-- reload code command
--
-- https://github.com/dramforever/vscode-ghc-simple
