module Screeps.FFI.Spawn
  ( BodyPart(..)
  , Spawn
  , SpawnStatus
  , isStructureSpawn
  , say
  , spawnCreep
  , spawnCreepWithMemory
  , spawnOk
  , spawning
  )
  where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (toUpper)
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Foreign (Foreign, isNull)
import Screeps.FFI.Room (Room, setRoomText)
import Screeps.FFI.RoomPosition (class HasRoomPosition, defaultPosition, pos)
import Screeps.FFI.Store (class HasStore)
import Screeps.FFI.Structure (class OfStructure, Structure, defaultOfStructure)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Spawn :: Type
instance HasRoomPosition Spawn where pos = defaultPosition 
instance HasStore Spawn where store s = (unsafeCoerce s).store

foreign import isStructureSpawn :: Structure -> Boolean

instance OfStructure Spawn where ofStructure = defaultOfStructure isStructureSpawn

foreign import work :: Foreign
foreign import move :: Foreign
foreign import carry :: Foreign


foreign import data SpawnStatus :: Type
foreign import spawnOk :: SpawnStatus
foreign import spawnStatusEq :: Fn2 SpawnStatus SpawnStatus Boolean
instance Eq SpawnStatus where eq a b = runFn2 spawnStatusEq a b
foreign import spawnCreepImpl :: EffectFn4 (Array Json) String Json Spawn SpawnStatus


data BodyPart = Work | Carry | Move
derive instance Generic BodyPart _
instance Show BodyPart where show part = toUpper $ genericShow part
instance EncodeJson BodyPart where 
  encodeJson Work = unsafeCoerce work
  encodeJson Move = unsafeCoerce move
  encodeJson Carry = unsafeCoerce carry


spawnCreepWithMemory :: ∀ a. EncodeJson a => Array BodyPart -> String -> a -> Spawn -> Effect SpawnStatus
spawnCreepWithMemory parts name memory spawn = 
  runEffectFn4 spawnCreepImpl (encodeJson <$> parts) name (encodeJson { memory }) spawn

spawnCreep :: Array BodyPart -> String -> Spawn -> Effect SpawnStatus
spawnCreep parts name spawn = 
  spawnCreepWithMemory parts name {} spawn

spawning :: Spawn -> Maybe { name :: String, needTime :: Int, remainingTime :: Int, cancel :: Effect Unit }
spawning spawn = if isNull currentlySpawning then Nothing else Just $ unsafeCoerce currentlySpawning
  where currentlySpawning = (unsafeCoerce spawn).spawning

room :: Spawn -> Room
room spawn = (unsafeCoerce spawn).room

say :: String -> Spawn -> Effect Unit
say msg spawn = setRoomText {msg, x, y} (room spawn)
  where 
  {x, y} = pos spawn