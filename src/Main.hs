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
import Data.HashMap.Strict
import Data.Maybe
import Data.Scientific (toBoundedInteger)
import Data.Text as T
import Data.Text.IO as T
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
            case lookup "findstart" cmd of
                Just (Number 0) -> do
                    let String base = fromJust $ lookup "base" cmd
                    results <- Array . V.fromList . Prelude.map fmtInfo <$> completeWithTypes ghci base
                    printf "base => %s\n" base
                    reply sock id' $ A.Object [("results", results)]
                Just (String "1") -> do -- XXX: Vim bug https://github.com/vim/vim/pull/2993
                    let String line = fromJust $ lookup "line" cmd
                        Number col  = fromJust $ lookup "column" cmd
                        (start, candidate) = findStart line (fromJust $ toBoundedInteger col)
                    printf "findstart => %d %s\n" start candidate
                    reply sock id' $ A.Object [("start", Number $ fromIntegral start)]
                Nothing -> error "Weird"
            serve sock ghci

  where fmtInfo (c, t, i) = A.Object [("word", String c), ("menu", String t), ("info", String i)]

ghciType :: Ghci -> Text -> IO Text
ghciType ghci expr = do
    Prelude.head <$> evalRpl ghci cmd
  where cmd = printf ":type %s" expr

ghciInfo :: Ghci -> Text -> IO [Text]
ghciInfo ghci expr = do
    evalRpl ghci cmd
  where cmd = printf ":info %s" expr

ghciComplete :: Ghci -> Text -> IO [Text]
ghciComplete ghci base = do
    candidates <- Prelude.tail <$> evalRpl ghci cmd
    return $ Prelude.map (T.init . T.tail) candidates
  where cmd = printf ":complete repl \"%s\"" base

data Candidate = Candidate
    { completion :: Text
    , type_ :: Text
    , info :: Text
    }


completeWithTypes :: Ghci -> Text -> IO [(Text, Text, Text)]
completeWithTypes ghci base = do
    candidates <- ghciComplete ghci base
    let candidates' = Prelude.take 10 candidates
    forM candidates' $ \c -> do
        t <- T.dropWhile isSpace . T.dropWhile (not . isSpace) <$> ghciType ghci c
        i <- T.unlines <$> ghciInfo ghci c
        return (c, t, i)

evalRpl :: Ghci -> String -> IO [Text]
evalRpl ghci cmd = Prelude.map T.pack <$> exec ghci cmd

findStart :: Text -> Int -> (Int, Text)
findStart line col =
    let (start, _) = T.splitAt (col - 1) line -- Column is [1..N] and column=X means text in [1..X-1]
     in trace (printf "debug: findStart \"%s\" %d" line col) $
        traceShowId $
        case parseCompletion start of
            Nothing -> (0, "")
            Just (Module mod loc) -> (startCol loc, mod)
            Just (Variable var loc) -> (startCol loc, var)
  where
    startCol (Loc _ c _ _) = c - 1 -- The text starts after the index X we return, so [X+1..]

data Completion = Module Text Loc
                | ModuleExport Text Loc
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
        (Nothing, Nothing) -> Nothing
        (Just tokens, Just locs) ->
            let tokens' = zip3 (Prelude.map fst tokens) (Prelude.map snd tokens) (Prelude.map snd locs)
             in decideCompletion tokens'
  where
    dropSpaces = Prelude.filter ((SpaceTok /=) . fst)

-- FIXME: type info for type class should be from 1st line from :info
-- :type-at
-- :set +c
-- : complete repl "import Foo.."
--
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:complete
--
-- reload code command
--
-- On complete module, instead of :info, use :browse
--
-- https://github.com/dramforever/vscode-ghc-simple
