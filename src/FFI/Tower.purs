module Screeps.FFI.Tower where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import FFI.Hits (class HasHits)
import Screeps.FFI.Creep (Creep)
import Screeps.FFI.RoomPosition (class HasRoomPosition)
import Screeps.FFI.Store (class HasStore)
import Screeps.FFI.Structure (class OfStructure, Structure, defaultOfStructure)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Tower :: Type
instance HasRoomPosition Tower
instance HasStore Tower where store s = (unsafeCoerce s).store

foreign import isStructureTower :: Structure -> Boolean

instance HasHits Tower
instance OfStructure Tower where ofStructure = defaultOfStructure isStructureTower

attack :: Creep -> Tower -> Effect Unit
attack creep tower = runEffectFn1 (unsafeCoerce tower).attack creep

heal :: Creep -> Tower -> Effect Unit
heal creep tower = runEffectFn1 (unsafeCoerce tower).heal creep

repair :: ∀ a. OfStructure a => a -> Tower -> Effect Unit
repair it tower = runEffectFn1 (unsafeCoerce tower).repair it


