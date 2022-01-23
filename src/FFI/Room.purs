module Screeps.FFI.Room where

import Prelude

import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (runEffectFn4)
import Foreign (isUndefined)
import Screeps.FFI.Controller (Controller)
import Screeps.FFI.Find (FindTarget)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Room :: Type

find :: ∀ findType. FindTarget findType -> Room -> Array findType
find target room = (unsafeCoerce room).find target

findWhere :: ∀ findType. FindTarget findType -> (findType -> Boolean) -> Room -> Array findType
findWhere target filter room = runFn2 (unsafeCoerce room).find target { filter }

controller :: Room -> Maybe Controller
controller room = 
  if isUndefined thisRoomController
  then Nothing
  else Just $ unsafeCoerce thisRoomController

  where
  thisRoomController = (unsafeCoerce room).controller 

energyAvailable :: Room -> Int 
energyAvailable room = (unsafeCoerce room).energyAvailable

energyCapacityAvailable :: Room -> Int 
energyCapacityAvailable room = (unsafeCoerce room).energyCapacityAvailable

setRoomText :: {msg :: String, x :: Int, y :: Int} -> Room -> Effect Unit 
setRoomText {msg, x, y} room = runEffectFn4 (unsafeCoerce room).visual.text msg x y {align: "left"}
