module Screeps.FFI.Tower where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Screeps.FFI.Creep (Creep)
import Screeps.FFI.RoomPosition (class HasRoomPosition, defaultPosition)
import Screeps.FFI.Store (class HasStore)
import Screeps.FFI.Structure (class OfStructure, Structure, defaultOfStructure)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Tower :: Type
instance HasRoomPosition Tower where pos = defaultPosition 
instance HasStore Tower where store s = (unsafeCoerce s).store

foreign import isStructureTower :: Structure -> Boolean

instance OfStructure Tower where ofStructure = defaultOfStructure isStructureTower

attack :: Creep -> Tower -> Effect Unit
attack creep tower = runEffectFn1 (unsafeCoerce tower).attack creep

repair :: ∀ a. OfStructure a => a -> Tower -> Effect Unit
repair it tower = runEffectFn1 (unsafeCoerce tower).repair it