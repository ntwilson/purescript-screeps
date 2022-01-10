module Screeps.Source where

import Screeps.RoomPosition (class HasRoomPosition)

foreign import data Source :: Type
instance HasRoomPosition Source
