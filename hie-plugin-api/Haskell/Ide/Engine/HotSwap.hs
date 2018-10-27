{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- | A data structure to define a hot-swappable plugin.
-- It is based on the FaceBook code made available at
-- https://github.com/fbsamples/ghc-hotswap
-- See also http://simonmar.github.io/posts/2017-10-17-hotswapping-haskell.html

module Haskell.Ide.Engine.HotSwap
  ( SOHandles(..)
  ) where

import Control.DeepSeq
import GHC.Generics
-- import Haskell.Ide.Engine.PluginsIdeMonads

-- ---------------------------------------------------------------------

-- | The set of functions that you want to expose from your shared object
data SOHandles = SOHandles
  -- { pluginDescriptor :: PluginId -> PluginDescriptor
  { pluginDescriptor :: String
  } deriving (Generic, NFData)
