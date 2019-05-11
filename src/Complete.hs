module Complete where

import RIO
import RIO.List
import RIO.List.Partial
import RIO.Text (Text)

import qualified RIO.Text as T
import qualified RIO.Text.Partial as T

import Control.Monad (forM, void)
import Data.Char
import Data.List (find)
import Data.Maybe
import Text.Printf

import GHC.SyntaxHighlighter
import Language.Haskell.Extension (KnownExtension)
import Language.Haskell.Ghcid

import App
import App.State
import Parse

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

ghciComplete :: Ghci -> Maybe Range -> Completion -> IO (Maybe ([Text], Bool))
ghciComplete ghci range compl = do
    matches <- evalExpr ghci $ cmd range
    return $ do
        cs <- matches
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

performCompletion :: MonadIO m => Ghci -> Maybe Range -> Completion -> m (Maybe ([Match], Bool))
performCompletion _ _ (Extension ext _) =
    let extensions = filter (T.isPrefixOf ext) ghcExtensions
     in return $ Just (map (\e -> Match e "" "") extensions, False)
performCompletion ghci range compl = do
    completion <- liftIO $ ghciComplete ghci range compl
    case completion of
        Just (matches, more) -> do
            matches' <-
                liftIO $
                forM matches $ \c
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
                    return $ Match c t (fromMaybe (error "performCompletion: ghci failed") i)
            return $ Just (matches', more)
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
