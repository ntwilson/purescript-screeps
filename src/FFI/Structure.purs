module Screeps.FFI.Structure where

import Prelude

import Data.Maybe (Maybe(..))
import Screeps.FFI.Find (FindTarget)
import Screeps.FFI.RoomPosition (class HasRoomPosition, defaultPosition)
import Screeps.FFI.Store (class HasStore)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Structure :: Type
foreign import find_structures :: FindTarget Structure
foreign import find_my_structures :: FindTarget Structure
instance HasRoomPosition Structure where pos = defaultPosition
instance HasStore Structure where 
  store s = (unsafeCoerce s).store

class OfStructure a where
  ofStructure :: Structure -> Maybe a

instance OfStructure Structure where ofStructure = Just 

defaultOfStructure :: ∀ a. (Structure -> Boolean) -> Structure -> Maybe a
defaultOfStructure structureIs struct = if structureIs struct then Just $ unsafeCoerce struct else Nothing 

toStructure :: ∀ a. OfStructure a => a -> Structure 
toStructure = unsafeCoerce

hits :: ∀ a. OfStructure a => a -> Int
hits str = (unsafeCoerce str).hits

hitsMax :: ∀ a. OfStructure a => a -> Int
hitsMax str = (unsafeCoerce str).hitsMax
