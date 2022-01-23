module Screeps.Spawning where

import Screeps.Prelude

import Screeps.FFI.Room as Room
import Screeps.FFI.Spawn (Spawn, spawnCreepWithMemory, spawnOk)
import Screeps.FFI.Spawn as Spawn
import Screeps.Worker (Job(..))

workerBodyParts :: Int -> Array BodyPart
workerBodyParts capacity 
  | capacity <= 300 = [Move, Carry, Work, Work]
  | capacity <= 350 = [Move, Move, Carry, Work, Work]
  | capacity <= 400 = [Move, Move, Carry, Carry, Work, Work]
  | capacity <= 500 = [Move, Move, Carry, Carry, Work, Work, Work]
  | capacity <= 550 = [Move, Move, Move, Carry, Carry, Work, Work, Work]
  | otherwise = [Move, Move, Move, Carry, Carry, Carry, Work, Work, Work]

spawnWorker :: Spawn -> Effect Unit
spawnWorker spawn = do
  {time} <- game
  let 
    name = i"Worker"time
    capacity = Room.energyCapacityAvailable (Spawn.room spawn)
  result <- spawnCreepWithMemory (workerBodyParts capacity) name Harvest spawn
  if result == spawnOk 
  then spawn # Spawn.say "Spawning Worker"
  else pure unit


