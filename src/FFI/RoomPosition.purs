module Screeps.FFI.RoomPosition where

import Prelude

import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..))
import Foreign (isNull)
import Screeps.FFI.Find (FindTarget)
import Unsafe.Coerce (unsafeCoerce)

type Pos = { x :: Int, y :: Int }

class HasRoomPosition a where
  pos :: a -> Pos

defaultPosition :: ∀ a. a -> Pos
defaultPosition it = (unsafeCoerce it).pos

findClosestByRange :: ∀ a b. HasRoomPosition a => FindTarget b -> a -> Maybe b
findClosestByRange target src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = (unsafeCoerce $ pos src).findClosestByRange target

findClosestByRangeWhere :: ∀ a b. HasRoomPosition a => FindTarget b -> (b -> Boolean) -> a -> Maybe b 
findClosestByRangeWhere target filter src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = runFn2 (unsafeCoerce $ pos src).findClosestByRange target { filter }

findClosestByPath :: ∀ a b. HasRoomPosition a => FindTarget b -> a -> Maybe b
findClosestByPath target src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = (unsafeCoerce $ pos src).findClosestByPath target

findClosestByPathWhere :: ∀ a b. HasRoomPosition a => FindTarget b -> (b -> Boolean) -> a -> Maybe b 
findClosestByPathWhere target filter src = if isNull findResult then Nothing else Just $ unsafeCoerce findResult
  where findResult = runFn2 (unsafeCoerce $ pos src).findClosestByPath target { filter }
