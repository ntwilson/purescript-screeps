module Main where

import Screeps.Prelude

import Data.Array as Array
import Effect.Exception (message, stack)
import FFI.Memory (memory)
import FFI.Memory as Memory
import Foreign.Object as Object
import Screeps.FFI.Controller as Controller
import Screeps.FFI.Creep (find_hostile_creeps)
import Screeps.FFI.Room as Room
import Screeps.FFI.Spawn as Spawn
import Screeps.Spawning (spawnWorker)
import Screeps.Tower (runTower, towers)
import Screeps.Worker (allWorkers, runWorker)
import Utils (orThrowError)

foreign import setLoop :: EffectFn1 (Effect Unit) Unit

main :: Effect Unit
main = runEffectFn1 setLoop do
  clearMemory
  spawn <- game >>= \{spawns} -> spawns # Array.fromFoldable # Array.head # orThrowError "No Spawns found" 
  let room = spawn # Spawn.room

  _ <- tryAndLog do
    case room # Room.controller, room # Room.find find_hostile_creeps # Array.head of
      Just ctrl, Just _enemy | Controller.canActivateSafeMode ctrl -> Controller.activateSafeMode ctrl
      _, _ -> pure unit

    let spawning = spawn # Spawn.spawning # isJust
    if spawning then pure unit else spawnWorker spawn

  workers <- allWorkers
  for_ workers $ tryAndLog <<< runWorker workers

  for_ (room # towers) $ tryAndLog <<< runTower

  where
  tryAndLog :: ∀ a. Effect a -> Effect Unit
  tryAndLog f = do
    try f >>= case _ of 
      Left e -> log $ message e <> fromMaybe "" (stack e)
      Right _ -> pure unit

  clearMemory = do
    {creeps:memCreeps} <- memory
    {creeps:gameCreeps} <- game
    for_ (Object.keys memCreeps) $ \creep -> 
      if not Object.member creep gameCreeps then Memory.deleteCreepByName creep else pure unit
