{-# LANGUAGE TemplateHaskell #-}
module TH.Logger where

import Control.Monad.Logger
import Control.Exception
import qualified Data.Text as T

log :: MonadLogger m => m ()
log = $logDebug $ T.pack $ displayException (undefined :: SomeException)
