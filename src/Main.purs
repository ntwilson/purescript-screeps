module Main where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Effect (Effect)
import Foreign.Object as Object
import Screeps.Creep (err_not_in_range, harvest, resource_energy)
import Screeps.Creep as Creep
import Screeps.Game (game)
import Screeps.Room (find_sources)
import Screeps.Room as Room
import Screeps.Store (store)
import Screeps.Store as Store
import Utils (orThrowError)

main :: Effect Unit
main = do
  for_ game.creeps $ \creep -> do

    freeCapacity <- creep # store # Store.getFreeCapacity
    if freeCapacity > 0 
    then do

      let sources = creep # Creep.room # Room.find find_sources
      source <- Array.head sources # orThrowError "No sources found in this room"
      result <- harvest source creep
      if result == err_not_in_range
      then creep # Creep.moveTo source
      else pure unit

    else do
      spawn <- Object.lookup "Spawn1" game.spawns # orThrowError "Couldn't find Spawn1"
      result <- creep # Creep.transfer spawn resource_energy 
      if result == err_not_in_range
      then creep # Creep.moveTo spawn
      else pure unit
