{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Prelude hiding (lookup, hGetLine)

import Control.Applicative
import Control.Exception (SomeException, bracket, try)
import Control.Monad (void, forM)
import Data.Aeson as A
import Data.ByteString.Char8 as B
import Data.HashMap.Strict
import Data.Maybe
import Data.Text as T
import System.IO hiding (hGetLine)
import GHC.IO.Handle.FD
import Data.Scientific (toBoundedInteger)
import Data.Char
import Text.Printf

import System.Posix.Files (createNamedPipe)
import Language.Haskell.Ghcid

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Simple.TCP as T
import qualified Data.Vector as V

main :: IO ()
main = do
    T.serve
      T.HostAny
      "8000"
      (\(sock, _) -> do

        print "- SERVE"
        (ghci, load) <- startGhci "ghci" Nothing printOutput
        serve sock ghci

        {-
         -bracket
         -  ((,) <$> openFileBlocking "ghci-in.fifo" ReadMode <*> openFileBlocking "ghci-out.fifo" WriteMode)
         -  (\(hIn, hOut) -> do hClose hIn >> hClose hOut)
         -  serve
         -}
      )
            where printOutput stream text = Prelude.putStrLn text

serve :: T.Socket -> Ghci -> IO ()
serve sock ghci = do
    print "- CMD"
    Just line <- T.recv sock 8096
    print line
    let msg = case eitherDecodeStrict line of
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
        Just (String "1") -> do -- XXX: Vim bug
            let String line = fromJust $ lookup "line" cmd
                Number col  = fromJust $ lookup "column" cmd
                (start, candidate) = findStart line (fromJust $ toBoundedInteger col)
            printf "findstart => %d %s\n" start candidate
            reply sock id' $ A.Object [("start", Number $ fromIntegral start)]
        Nothing -> error "Weird"
    serve sock ghci

  where fmtInfo (c, t, i) = A.Object [("word", String c), ("menu", String t), ("info", String i)]

type_ :: Ghci -> Text -> IO Text
type_ ghci val = do
    Prelude.head <$> evalRpl ghci cmd
  where cmd = printf ":type %s" val

info :: Ghci -> Text -> IO [Text]
info ghci val = do
    evalRpl ghci cmd
  where cmd = printf ":info %s" val

complete :: Ghci -> Text -> IO [Text]
complete ghci base = do
    candidates <- Prelude.tail <$> evalRpl ghci cmd
    return $ Prelude.map (T.init . T.tail) candidates
  where cmd = printf ":complete repl \"%s\"" base

completeWithTypes :: Ghci -> Text -> IO [(Text, Text, Text)]
completeWithTypes ghci base = do
    candidates <- complete ghci base
    forM candidates $ \c -> do
        t <- T.dropWhile isSpace . T.dropWhile (not . isSpace) <$> type_ ghci c
        i <- T.unlines <$> info ghci c
        return (c, t, i)

evalRpl :: Ghci -> String -> IO [Text]
evalRpl ghci cmd = Prelude.map T.pack <$> exec ghci cmd

reply :: T.Socket -> Int -> Value -> IO ()
reply sock id' resp = do
    Prelude.putStrLn "reply"
    T.send sock . BL.toStrict . encode . Array $ V.fromList [Number $ fromIntegral id', resp]

findStart :: Text -> Int -> (Int, Text)
findStart "" 0 = (0, "")
findStart line col 
  | T.length line <= col = findStart line (col - 1)
  | (start, _) <- T.splitAt len line
  , (candidate, _) <- T.span validRest $ T.reverse start
  , T.length candidate /= 0
  = (col - (T.length candidate - 1), T.reverse candidate)
  | otherwise = (col, "")
  where len = col + 1
        validRest c = isAlphaNum c || c == '_' || c == '\''
        validFirst c = isLower c || c == '_'

-- FIXME: type info for type class should be from 1st line from :info
-- :type-at
-- :set +c
-- : complete repl "import Foo.."
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:complete
--
-- reload code, reconnect in vim
--
-- https://github.com/dramforever/vscode-ghc-simple
