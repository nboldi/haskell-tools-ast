{-# LANGUAGE CPP #-}
module CPP.ConditionalImport where

import Control.Applicative ((<$>))
import Data.Maybe (Maybe(..), isJust)
#ifndef USE_DATA_LIST
import Control.Monad (Monad(..))
#endif

a = isJust $ id <$> (Nothing >> Nothing)
