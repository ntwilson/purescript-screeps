module Screeps.FFI.Source where

import Screeps.FFI.Find (FindTarget)
import Screeps.FFI.RoomPosition (class HasRoomPosition)

foreign import data Source :: Type
foreign import find_sources :: FindTarget Source
instance HasRoomPosition Source
