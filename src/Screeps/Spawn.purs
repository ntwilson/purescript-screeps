module Screeps.Spawn
  ( BodyPart(..)
  , Spawn
  , SpawnStatus
  , ofStructure
  , spawnCreep
  , spawnCreepWithMemory
  )
  where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (toUpper)
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Screeps.RoomPosition (class HasRoomPosition)
import Screeps.Store (class HasStore)
import Screeps.Structure (Structure)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Spawn :: Type
instance HasRoomPosition Spawn
instance HasStore Spawn where store s = (unsafeCoerce s).store

foreign import isStructureSpawn :: Structure -> Boolean

ofStructure :: Structure -> Maybe Spawn
ofStructure struct = if isStructureSpawn struct then Just $ unsafeCoerce struct else Nothing 


foreign import data SpawnStatus :: Type
foreign import spawnCreepImpl :: EffectFn4 (Array String) String Json Spawn SpawnStatus


data BodyPart = Work | Carry | Move
derive instance Generic BodyPart _
instance Show BodyPart where 
  show part = toUpper $ genericShow part

spawnCreepWithMemory :: ∀ a. EncodeJson a => Array BodyPart -> String -> a -> Spawn -> Effect SpawnStatus
spawnCreepWithMemory parts name memory spawn = 
  runEffectFn4 spawnCreepImpl (show <$> parts) name (encodeJson { memory }) spawn 

spawnCreep :: Array BodyPart -> String -> Spawn -> Effect SpawnStatus
spawnCreep parts name spawn = 
  spawnCreepWithMemory parts name {} spawn
