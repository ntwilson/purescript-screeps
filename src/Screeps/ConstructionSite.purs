module Screeps.ConstructionSite where

import Screeps.RoomPosition (class HasRoomPosition)

foreign import data ConstructionSite :: Type
instance HasRoomPosition ConstructionSite
