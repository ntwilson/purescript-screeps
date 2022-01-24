module Screeps.FFI.Controller where

import Prelude

import Effect (Effect)
import FFI.Hits (class HasHits)
import Screeps.FFI.RoomPosition (class HasRoomPosition)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Controller :: Type
  
instance HasHits Controller
instance HasRoomPosition Controller

activateSafeMode :: Controller -> Effect Unit
activateSafeMode controller = (unsafeCoerce controller).activateSafeMode

canActivateSafeMode :: Controller -> Boolean
canActivateSafeMode controller = (unsafeCoerce controller).safeModeAvailable > 0

my :: Controller -> Boolean
my controller = (unsafeCoerce controller).my

