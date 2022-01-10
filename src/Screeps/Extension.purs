module Screeps.Extension
  ( Extension
  , ofStructure
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Screeps.Store (class HasStore)
import Screeps.Structure (Structure)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Extension :: Type
instance HasStore Extension where store s = (unsafeCoerce s).store

foreign import isStructureExtension :: Structure -> Boolean

ofStructure :: Structure -> Maybe Extension 
ofStructure struct = if isStructureExtension struct then Just $ unsafeCoerce struct else Nothing
