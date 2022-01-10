module Screeps.Room where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (isUndefined)
import Screeps.ConstructionSite (ConstructionSite)
import Screeps.Controller (Controller)
import Screeps.Source (Source)
import Screeps.Structure (Structure)
import Unsafe.Coerce (unsafeCoerce)

foreign import data FindTarget :: Type -> Type 

foreign import find_sources :: FindTarget Source
foreign import find_construction_sites :: FindTarget ConstructionSite
foreign import find_structures :: FindTarget Structure

foreign import data Room :: Type

find :: ∀ findType. FindTarget findType -> Room -> Array findType
find target room = (unsafeCoerce room).find target

findWhere :: ∀ findType. FindTarget findType -> (findType -> Boolean) -> Room -> Array findType
findWhere target filter room = (unsafeCoerce room).find target { filter }

controller :: Room -> Maybe Controller
controller room = 
  if isUndefined thisRoomController
  then Nothing
  else Just $ unsafeCoerce thisRoomController

  where
  thisRoomController = (unsafeCoerce room).controller 

