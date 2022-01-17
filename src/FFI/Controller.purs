module Screeps.FFI.Controller where

import Prelude

import Effect (Effect)
import Screeps.FFI.RoomPosition (class HasRoomPosition, defaultPosition)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Controller :: Type
instance HasRoomPosition Controller where pos = defaultPosition 

activateSafeMode :: Controller -> Effect Unit
activateSafeMode controller = (unsafeCoerce controller).activateSafeMode
