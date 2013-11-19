module Baskerville.Utilities.Control where

import Control.Monad
import Data.Maybe

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (return ())
