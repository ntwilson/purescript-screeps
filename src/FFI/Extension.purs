module Screeps.FFI.Extension
  ( Extension
  , isStructureExtension
  )
  where

import FFI.Hits (class HasHits)
import Screeps.FFI.Store (class HasStore)
import Screeps.FFI.Structure (class OfStructure, Structure, defaultOfStructure)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Extension :: Type
instance HasStore Extension where store s = (unsafeCoerce s).store

foreign import isStructureExtension :: Structure -> Boolean

instance HasHits Extension
instance OfStructure Extension where ofStructure = defaultOfStructure isStructureExtension
