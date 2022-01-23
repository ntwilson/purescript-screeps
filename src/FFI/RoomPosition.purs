module Screeps.FFI.RoomPosition where

import Prelude

import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..))
import Foreign (isNull)
import Screeps.FFI.Find (FindTarget)
import Unsafe.Coerce (unsafeCoerce)

class HasRoomPosition :: Type -> Constraint
class HasRoomPosition a 

findClosestByRange :: ∀ a b. HasRoomPosition a => FindTarget b -> a -> Maybe b
findClosestByRange target src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = (unsafeCoerce $ src).pos.findClosestByRange target

findClosestByRangeWhere :: ∀ a b. HasRoomPosition a => FindTarget b -> (b -> Boolean) -> a -> Maybe b 
findClosestByRangeWhere target filter src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = runFn2 (unsafeCoerce $ src).pos.findClosestByRange target { filter }

findClosestByPath :: ∀ a b. HasRoomPosition a => FindTarget b -> a -> Maybe b
findClosestByPath target src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = (unsafeCoerce $ src).pos.findClosestByPath target

findClosestByPathWhere :: ∀ a b. HasRoomPosition a => FindTarget b -> (b -> Boolean) -> a -> Maybe b 
findClosestByPathWhere target filter src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = runFn2 (unsafeCoerce $ src).pos.findClosestByPath target { filter }
