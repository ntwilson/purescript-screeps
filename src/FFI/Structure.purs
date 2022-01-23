module Screeps.FFI.Structure where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import FFI.Hits (class HasHits)
import Screeps.FFI.Controller (Controller)
import Screeps.FFI.Find (FindTarget)
import Screeps.FFI.Room (Room)
import Screeps.FFI.Room as Room
import Screeps.FFI.RoomPosition (class HasRoomPosition)
import Screeps.FFI.Store (class HasStore)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Structure :: Type
foreign import find_structures :: FindTarget Structure
foreign import find_my_structures :: FindTarget Structure
instance HasHits Structure
instance HasRoomPosition Structure
instance HasStore Structure where 
  store s = (unsafeCoerce s).store

class HasHits a <= OfStructure a where
  ofStructure :: Structure -> Maybe a

instance OfStructure Structure where ofStructure = Just 

defaultOfStructure :: ∀ a. (Structure -> Boolean) -> Structure -> Maybe a
defaultOfStructure structureIs struct = if structureIs struct then Just $ unsafeCoerce struct else Nothing 

toStructure :: ∀ a. OfStructure a => a -> Structure 
toStructure = unsafeCoerce

foreign import isStructureController :: Structure -> Boolean
instance OfStructure Controller where ofStructure = defaultOfStructure isStructureController 

structures :: ∀ struct. OfStructure struct => Room -> Array struct
structures room = 
  room # Room.find find_my_structures
  # Array.mapMaybe ofStructure


