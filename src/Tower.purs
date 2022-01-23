module Screeps.Tower where

import Screeps.Prelude

import FFI.Hits (hits, hitsMax)
import Screeps.FFI.Creep (find_hostile_creeps, find_my_creeps)
import Screeps.FFI.Room (Room)
import Screeps.FFI.RoomPosition as RoomPosition
import Screeps.FFI.Structure (structures)
import Screeps.FFI.Tower (Tower)
import Screeps.FFI.Tower as Tower

towers :: Room -> Array Tower
towers = structures 

runTower :: Tower -> Effect Unit
runTower tower = 
  case hostiles, woundedFriendlies of 
    Just enemy, _ -> Tower.attack enemy tower
    _, Just friend -> Tower.heal friend tower
    Nothing, Nothing -> pure unit

  where 
    hostiles = tower # RoomPosition.findClosestByRange find_hostile_creeps
    woundedFriendlies = tower # RoomPosition.findClosestByRangeWhere find_my_creeps \creep -> hits creep < hitsMax creep
