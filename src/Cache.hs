module Cache where

import RIO
import RIO.State

import qualified Data.Map as M

type InfoCache = Map Text Text

emptyCache :: InfoCache
emptyCache = M.empty

cacheInfo :: (MonadIO m, MonadState InfoCache m) => Text -> Text -> m ()
cacheInfo k v = modify $ M.insert k v

getCached :: (MonadIO m, MonadState InfoCache m) => Text -> m (Maybe Text)
getCached k = M.lookup k <$> get
