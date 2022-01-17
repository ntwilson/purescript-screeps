module Screeps.FFI.Game where

import Effect (Effect)
import Foreign.Object (Object)
import Screeps.FFI.Creep (Creep)
import Screeps.FFI.Spawn (Spawn)

type Game = 
  { spawns :: Object Spawn 
  , creeps :: Object Creep
  , time :: Int
  }

foreign import game :: Game

type Memory = 
  { creeps :: Object Creep }

foreign import memory :: Effect Memory
