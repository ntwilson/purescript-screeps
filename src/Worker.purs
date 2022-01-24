module Screeps.Worker where

import Screeps.Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Int as Int
import Data.Lazy (defer)
import FFI.Hits (hits, hitsMax, hitsPercent)
import Foreign.Object as Object
import Screeps.FFI.ConstructionSite (ConstructionSite)
import Screeps.FFI.Controller as Controller
import Screeps.FFI.Creep (err_not_in_range, ok)
import Screeps.FFI.Creep as Creep
import Screeps.FFI.Room as Room
import Screeps.FFI.RoomPosition as RoomPosition
import Screeps.FFI.Spawn as Spawn
import Screeps.FFI.Store as Store
import Screeps.FFI.Structure (Structure)
import Utils (noteL, orThrowError)

data Job = Harvest | StoreEnergy | Build | Upgrade
allJobs :: Array Job
allJobs = [Harvest , StoreEnergy , Build , Upgrade]
derive instance Eq Job
derive instance Generic Job _
instance Show Job where show = genericShow
instance EncodeJson Job where 
  encodeJson job = encodeJson $ Object.singleton (show job) {}
instance DecodeJson Job where 
  decodeJson json = do
    obj <- Json.toObject json # note (TypeMismatch "Object")
    allJobs 
      # Array.find (\job -> Object.member (show job) obj) 
      # noteL (defer \_ -> (AtKey (intercalate " | " (show <$> allJobs)) MissingValue))

type WorkerMem = {job :: Job}

whatJobShouldIPick :: Int -> Int -> Int -> Creep -> Job
whatJobShouldIPick nHarvesters nBuilders nUpgraders creep = go
  where 
  nEmployedCreeps = nHarvesters + nBuilders + nUpgraders
  
  go
    | (creep # store # Store.usedCapacity resource_energy) == 0 = Harvest
    | nHarvesters < 1 = StoreEnergy
    | nBuilders < 1 
    , isJust (anythingToBuild creep) || isJust (anythingWorthRepairing creep) = Build
    | nUpgraders < 1 = Upgrade
    | Int.toNumber nHarvesters / Int.toNumber nEmployedCreeps < 0.2 = StoreEnergy
    | Int.toNumber nBuilders / Int.toNumber nEmployedCreeps < 0.35 
    , isJust (anythingToBuild creep) || isJust (anythingAtAllToRepair creep) = Build
    | otherwise = Upgrade

anythingWorthRepairing :: Creep -> Maybe Structure
anythingWorthRepairing creep = 
  creep # Creep.room 
  # Room.findWhere find_my_structures 
    (\struct -> case hitsPercent struct of 
      Just percent -> percent < 0.5 || hits struct < 1_000
      Nothing -> false
    )
  # Array.sortBy (comparing hitsPercent)
  # Array.head 

anythingToBuild :: Creep -> Maybe ConstructionSite
anythingToBuild creep = creep # Creep.room # Room.find find_construction_sites # Array.head

anythingAtAllToRepair :: Creep -> Maybe Structure
anythingAtAllToRepair creep = 
  creep # Creep.room # Room.findWhere find_my_structures (\struct -> hits struct < hitsMax struct && hits struct < 20_000)
  # Array.sortBy (comparing hitsPercent)
  # Array.head 

runWorker :: Array {creep::Creep, memory::Job} -> {creep::Creep, memory::Job} -> Effect Unit 
runWorker allCreeps {creep, memory} = case memory of 
  Harvest | (creep # store # Store.freeCapacity resource_energy) == 0 -> findAJob
  Harvest | otherwise -> harvestEnergy
  _ | (creep # store # Store.usedCapacity resource_energy) == 0 -> findAJob

  StoreEnergy -> depositEnergy
  Build -> buildSomething
  Upgrade -> upgradeSomething

  where
  harvestEnergy =
    case creep # RoomPosition.findClosestByPath find_sources of
      Nothing -> stuck
      Just source -> do
        result <- Creep.harvest source creep
        if result == err_not_in_range
        then creep # Creep.moveToAndVisualize source "#ffaa00"
        else pure unit

  findAJob =
    let 
      harvesters = allCreeps # Array.filter \{memory} -> memory == StoreEnergy
      builders = allCreeps # Array.filter \{memory} -> memory == Build 
      upgraders = allCreeps # Array.filter \{memory} -> memory == Upgrade

    in case whatJobShouldIPick (Array.length harvesters) (Array.length builders) (Array.length upgraders) creep of
      StoreEnergy -> do
        creep # Creep.say "📦 depositing"
        creep # Creep.setMem StoreEnergy

      Build -> do
        creep # Creep.say "🚧 build"
        creep # Creep.setMem Build

      Upgrade -> do
        creep # Creep.say "⚡ upgrade"
        creep # Creep.setMem Upgrade

      Harvest -> do
        creep # Creep.say "🔄 harvest"
        creep # Creep.setMem Harvest

  depositEnergy = do
    spawn <- game >>= \{spawns} -> spawns # Array.fromFoldable # Array.head # orThrowError "No Spawns found" 
    let 
      tower = creep # RoomPosition.findClosestByPathWhere find_my_structures \structure ->
        (structure # isStructureTower)
        && 
        (store structure # Store.freeCapacity resource_energy) > 0

      targetsInRoom = creep # RoomPosition.findClosestByPathWhere find_my_structures \structure ->
        any (_ $ structure) [isStructureSpawn, isStructureExtension] 
        && 
        (store structure # Store.freeCapacity resource_energy) > 0

      targetsBySpawn = spawn # RoomPosition.findClosestByPathWhere find_my_structures \structure ->
        any (_ $ structure) [isStructureSpawn, isStructureExtension] 
        && 
        (store structure # Store.freeCapacity resource_energy) > 0

    case tower, targetsInRoom, targetsBySpawn of 
      Nothing, Nothing, Nothing -> stuck
      Just target, _, _ -> depositTo target
      _, Just target, _ -> depositTo target
      _, _, Just target -> depositTo target

    where 
    depositTo target = do
      result <- creep # Creep.transfer target resource_energy 
      if result == err_not_in_range
      then creep # Creep.moveToAndVisualize target "#ffffff"
      else pure unit

  upgradeSomething = do
    spawn <- game >>= \{spawns} -> spawns # Array.fromFoldable # Array.head # orThrowError "No Spawns found" 
    case creep # Creep.room # Room.controller, spawn # Spawn.room # Room.controller of 
      Just controller, _ | controller # Controller.my -> do
        result <- creep # Creep.upgradeController controller
        if result == err_not_in_range 
        then creep # Creep.moveToAndVisualize controller "#ffffff"
        else pure unit
      _, Just controller -> do 
        result <- creep # Creep.upgradeController controller
        if result == err_not_in_range 
        then creep # Creep.moveToAndVisualize controller "#ffffff"
        else pure unit
      _, _ -> stuck

  buildSomething = 
    case anythingWorthRepairing creep, anythingToBuild creep, anythingAtAllToRepair creep of
      Just target, _, _ -> repair target 
      _, Just target, _ -> build target
      _, _, Just target -> repair target
      Nothing, Nothing, Nothing -> stuck

  stuck = do
    creep # Creep.say "☹️ stuck"
    findAJob

  repair struct = do
    result <- creep # Creep.repair struct
    if result /= ok
    then creep # Creep.moveToAndVisualize struct "#ffffff"
    else pure unit

  build struct = do
    result <- creep # Creep.build struct
    if result /= ok
    then creep # Creep.moveToAndVisualize struct "#ffffff"
    else pure unit

allWorkers :: Effect (Array {creep :: Creep, memory :: Job})
allWorkers = game >>= \{creeps} -> Array.fromFoldable creeps # traverse folder <#> Array.mapMaybe hush 
  where 
    folder creep = do
      result <- runExceptT $ Creep.getMem creep
      case result of 
        Right (memory :: Job) -> pure $ Right { creep, memory }
        Left err -> 
          log ("Unable to access the memory of " <> Creep.name creep <> ". " <> printJsonDecodeError err) $> Left err

