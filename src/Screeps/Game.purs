module Screeps.Game where

import Foreign.Object (Object)
import Screeps.Creep (Creep)
import Screeps.Spawn (Spawn)

type Game = 
  { spawns :: Object Spawn 
  , creeps :: Object Creep
  }

foreign import game :: Game
