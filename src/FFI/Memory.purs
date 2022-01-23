module FFI.Memory where

import Prelude
import Effect (Effect)
import Foreign.Object (Object)
import Screeps.FFI.Creep (Creep)

type Memory = { creeps :: Object Creep }

foreign import memory :: Effect Memory
foreign import deleteCreepByName :: String -> Effect Unit