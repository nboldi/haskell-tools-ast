{-# LANGUAGE CPP #-}
module CPP.ConditionalImport where

import Data.Maybe
import Control.Applicative
#ifndef USE_DATA_LIST
import Control.Monad
#endif
import Data.List

a = isJust $ id <$> (Nothing >> Nothing)
