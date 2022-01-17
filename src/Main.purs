module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson, printJsonDecodeError, stringify)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldM, for_)
import Data.Generic.Rep (class Generic)
import Data.Interpolate (i)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Screeps.FFI.ConstructionSite (find_construction_sites)
import Screeps.FFI.Creep (Creep, err_not_in_range, getMem, harvest, ok)
import Screeps.FFI.Creep as Creep
import Screeps.FFI.Extension (isStructureExtension)
import Screeps.FFI.Game (game)
import Screeps.FFI.Room as Room
import Screeps.FFI.RoomPosition as RoomPosition
import Screeps.FFI.Source (find_sources)
import Screeps.FFI.Spawn (BodyPart(..), isStructureSpawn, spawnCreepWithMemory, spawnOk)
import Screeps.FFI.Spawn as Spawn
import Screeps.FFI.Store (resource_energy, store)
import Screeps.FFI.Store as Store
import Screeps.FFI.Structure (find_my_structures)
import Screeps.FFI.Tower (isStructureTower)
import Utils (orThrowError)

foreign import setLoop :: EffectFn1 (Effect Unit) Unit

type HarvesterMem = {depositing :: Boolean}
type BuilderMem = {building :: Boolean}
type UpgraderMem = {upgrading :: Boolean}
data Role = Harvester HarvesterMem | Builder BuilderMem | Upgrader UpgraderMem

derive instance Eq Role
derive instance Generic Role _
instance Show Role where show = genericShow
instance EncodeJson Role where 
  encodeJson (Harvester h) = encodeJson {"Harvester": h}
  encodeJson (Builder b) = encodeJson {"Builder": b}
  encodeJson (Upgrader u) = encodeJson {"Upgrader": u}
instance DecodeJson Role where 
  decodeJson json
    | Right (h::{"Harvester" :: HarvesterMem}) <- decodeJson json = Right $ Harvester h."Harvester"
    | Right (b::{"Builder" :: BuilderMem}) <- decodeJson json = Right $ Builder b."Builder"
    | Right (u::{"Upgrader" :: UpgraderMem}) <- decodeJson json = Right $ Upgrader u."Upgrader"
    | otherwise = Left $ Named (stringify json) $ TypeMismatch $ "Expected object with a single tag of 'Harvester'|'Builder'|'Upgrader'"

creepsOfRole :: ∀ a. (Role -> Maybe a) -> Effect (List {creep :: Creep, memory :: a}) 
creepsOfRole projection = List.fromFoldable game.creeps # foldM folder Nil
  where 
    folder creeps creep = do
      result <- runExceptT $ getMem creep
      case result of 
        Right (role :: Role) | Just memory <- projection role -> pure $ { creep, memory } : creeps
        Right _ -> pure creeps
        Left err -> 
          log ("Unable to access the memory of " <> Creep.name creep <> ". " <> printJsonDecodeError err) $> creeps

asHarvester :: Role -> Maybe HarvesterMem
asHarvester (Harvester h) = Just h
asHarvester _ = Nothing

asBuilder :: Role -> Maybe BuilderMem
asBuilder (Builder b) = Just b
asBuilder _ = Nothing

asUpgrader :: Role -> Maybe UpgraderMem
asUpgrader (Upgrader u) = Just u
asUpgrader _ = Nothing

-- checkRoleOrLog :: Role -> Creep -> Effect Boolean
-- checkRoleOrLog role creep = do
--   result <- runExceptT $ isRole role creep
--   case result of 
--     Right b -> pure b 
--     Left err -> 
--       log ("Unable to access the memory of " <> Creep.name creep <> ". " <> printJsonDecodeError err) $> false

-- isRole :: Role -> Creep -> ExceptT JsonDecodeError Effect Boolean
-- isRole targetRole creep = do
--   {role} :: {role::Role} <- getMem creep
--   pure $ role == targetRole

harvestEnergy :: Creep -> Effect Unit
harvestEnergy creep = do
  case creep # RoomPosition.findClosestByPath find_sources of
    Nothing -> creep # Creep.say "☹️ stuck"
    Just source -> do
      result <- harvest source creep
      if result == err_not_in_range
      then creep # Creep.moveToAndVisualize source "#ffaa00"
      else pure unit

runHarvester :: {creep::Creep, memory::HarvesterMem} -> Effect Unit 
runHarvester {creep, memory} = go
  where
  go 
    | memory.depositing && (creep # store # Store.usedCapacity resource_energy) == 0 = do
      creep # Creep.say "🔄 harvest"
      creep # Creep.setMem (Harvester {depositing:false})
      harvestEnergy creep
    | not memory.depositing && (creep # store # Store.freeCapacity resource_energy) == 0 = do
      creep # Creep.say "📦 depositing"
      creep # Creep.setMem (Harvester {depositing:true})
      depositSomething 
    | memory.depositing = depositSomething
    | otherwise = harvestEnergy creep

  depositSomething = 
    let 
      targets = creep # RoomPosition.findClosestByPathWhere find_my_structures \structure ->
        any (_ $ structure) [isStructureTower, isStructureSpawn, isStructureExtension] 
        && 
        ((store structure # Store.freeCapacity resource_energy) > 0)

    in case targets of 
      Nothing -> creep # Creep.say "☹️ stuck"
      Just target -> do
        result <- creep # Creep.transfer target resource_energy 
        if result == err_not_in_range
        then creep # Creep.moveToAndVisualize target "#ffffff"
        else pure unit
    
runUpgrader :: {creep::Creep, memory::UpgraderMem} -> Effect Unit
runUpgrader {creep, memory} = go 
  where 
  go 
    | memory.upgrading && (creep # store # Store.usedCapacity resource_energy) == 0 = do
      creep # Creep.say "🔄 harvest"
      creep # Creep.setMem (Upgrader {upgrading:false})
      harvestEnergy creep
    | not memory.upgrading && (creep # store # Store.freeCapacity resource_energy) == 0 = do
      creep # Creep.say "⚡ upgrade"
      creep # Creep.setMem (Upgrader {upgrading:true})
      upgradeSomething 
    | memory.upgrading = upgradeSomething
    | otherwise = harvestEnergy creep

  upgradeSomething = 
    case creep # Creep.room # Room.controller of 
      Nothing -> creep # Creep.say "☹️ stuck"
      Just controller -> do
        result <- creep # Creep.upgradeController controller
        if result == err_not_in_range 
        then creep # Creep.moveToAndVisualize controller "#ffffff"
        else pure unit

runBuilder :: {creep::Creep, memory::BuilderMem} -> Effect Unit
runBuilder {creep, memory} = go
  where 
  go
    | memory.building && (creep # store # Store.usedCapacity resource_energy) == 0 = do
      creep # Creep.say "🔄 harvest"
      creep # Creep.setMem (Builder {building:false})
      harvestEnergy creep
    | not memory.building && (creep # store # Store.freeCapacity resource_energy) == 0 = do
      creep # Creep.say "🚧 build"
      creep # Creep.setMem (Builder {building:true})
      buildSomething
    | memory.building = buildSomething
    | otherwise = harvestEnergy creep

  buildSomething = 
    let targets = creep # Creep.room # Room.find find_construction_sites
    in case Array.head targets of 
      Nothing -> creep # Creep.say "☹️ stuck"
      Just target -> do
        result <- creep # Creep.build target
        if result /= ok
        then creep # Creep.moveToAndVisualize target "#ffffff"
        else pure unit

main :: Effect Unit
main = runEffectFn1 setLoop $ do
  spawn <- game.spawns # Array.fromFoldable # Array.head # orThrowError "No Spawns found" 

  harvesters <- creepsOfRole asHarvester 
  builders <- creepsOfRole asBuilder
  upgraders <- creepsOfRole asUpgrader

  let spawning = spawn # Spawn.spawning # isJust
  if spawning then pure unit
  else if List.length harvesters < 5 then spawnHarvester spawn
  else if List.length upgraders < 5 then spawnUpgrader spawn
  else if List.length builders < 5 then spawnBuilder spawn
  else spawn # Spawn.say "plenty of creeps 😊"

  for_ harvesters runHarvester
  for_ builders runBuilder
  for_ upgraders runUpgrader

  where 
  spawnHarvester spawn = do
    result <- spawnCreepWithMemory [Move, Carry, Work, Work] name (Harvester {depositing:false}) spawn
    if result == spawnOk 
    then spawn # Spawn.say "Spawning Harvester"
    else pure unit

    where 
    name = i"Harvester"game.time

  spawnBuilder spawn = do
    result <- spawnCreepWithMemory [Move, Carry, Work, Work] name (Builder {building:false}) spawn
    if result == spawnOk 
    then spawn # Spawn.say "Spawning Builder"
    else pure unit

    where 
    name = i"Builder"game.time

  spawnUpgrader spawn = do
    result <- spawnCreepWithMemory [Move, Carry, Work, Work] name (Upgrader {upgrading:false}) spawn
    if result == spawnOk 
    then spawn # Spawn.say "Spawning Upgrader"
    else pure unit

    where 
    name = i"Upgrader"game.time
