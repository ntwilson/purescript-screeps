module Screeps.Store where

import Effect (Effect)

foreign import data Store :: Type 
foreign import getFreeCapacity :: Store -> Effect Int

class HasStore a where
  store :: a -> Store 

