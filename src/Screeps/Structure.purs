module Screeps.Structure where

import Screeps.Store (class HasStore)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Structure :: Type
instance HasStore Structure where 
  store s = (unsafeCoerce s).store
