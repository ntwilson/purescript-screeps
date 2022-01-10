module Screeps.Controller where

import Screeps.RoomPosition (class HasRoomPosition)

foreign import data Controller :: Type
instance HasRoomPosition Controller
