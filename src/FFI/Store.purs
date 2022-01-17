module Screeps.FFI.Store
  ( Resource
  , Store
  , class HasStore
  , freeCapacity
  , resource_energy
  , store
  , usedCapacity
  )
  where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2)

foreign import data Resource :: Type
instance Eq Resource where eq = runFn2 resourceEq
foreign import resourceEq :: Fn2 Resource Resource Boolean
foreign import resource_energy :: Resource
foreign import data Store :: Type 
foreign import freeCapacityImpl :: Fn2 Resource Store Int
foreign import usedCapacityImpl :: Fn2 Resource Store Int

freeCapacity :: Resource -> Store -> Int
freeCapacity = runFn2 freeCapacityImpl

usedCapacity :: Resource -> Store -> Int
usedCapacity = runFn2 usedCapacityImpl

class HasStore a where
  store :: a -> Store 

