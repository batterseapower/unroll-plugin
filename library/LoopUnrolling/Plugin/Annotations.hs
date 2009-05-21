module LoopUnrolling.Plugin.Annotations where

import Data.Data
import Data.Typeable


data Peel = Peel Int deriving (Data, Typeable)

data Unroll = Unroll Int deriving (Data, Typeable)