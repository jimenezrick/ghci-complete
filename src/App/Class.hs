module App.Class where

import RIO

class HasLogFunc env =>
      HasLogOpts env
    where
    logOptsL :: Lens' env LogOptions
