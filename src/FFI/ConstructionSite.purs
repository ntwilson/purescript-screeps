module Screeps.FFI.ConstructionSite where

import Screeps.FFI.Find (FindTarget)
import Screeps.FFI.RoomPosition (class HasRoomPosition, defaultPosition)

foreign import data ConstructionSite :: Type
foreign import find_construction_sites :: FindTarget ConstructionSite
instance HasRoomPosition ConstructionSite where pos = defaultPosition 
