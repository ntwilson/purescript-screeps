module Screeps.FFI.Creep
  ( Creep(..)
  , StatusCode
  , build
  , err_not_in_range
  , find_hostile_creeps
  , getMem
  , harvest
  , moveTo
  , moveToAndVisualize
  , name
  , ok
  , room
  , say
  , setMem
  , suicide
  , transfer
  , upgradeController
  )
  where

import Prelude

import Control.Monad.Except (ExceptT, except, lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn1, runEffectFn2)
import Screeps.FFI.ConstructionSite (ConstructionSite)
import Screeps.FFI.Controller (Controller)
import Screeps.FFI.Find (FindTarget)
import Screeps.FFI.Room (Room)
import Screeps.FFI.RoomPosition (class HasRoomPosition, defaultPosition)
import Screeps.FFI.Source (Source)
import Screeps.FFI.Store (class HasStore, Resource)
import Unsafe.Coerce (unsafeCoerce)

foreign import data StatusCode :: Type
instance Eq StatusCode where
  eq = runFn2 statusCodeEq
foreign import err_not_in_range :: StatusCode
foreign import ok :: StatusCode
foreign import statusCodeEq :: Fn2 StatusCode StatusCode Boolean

foreign import data Creep :: Type
instance HasRoomPosition Creep where pos = defaultPosition 
instance HasStore Creep where store s = (unsafeCoerce s).store

foreign import find_hostile_creeps :: FindTarget Creep

room :: Creep -> Room 
room = unsafeCoerce >>> _.room

harvest :: Source -> Creep -> Effect StatusCode
harvest src creep = runEffectFn1 (unsafeCoerce creep).harvest src

moveTo :: ∀ a. HasRoomPosition a => a -> Creep -> Effect Unit
moveTo target creep = 
  runEffectFn1 (unsafeCoerce creep).moveTo target

moveToAndVisualize :: ∀ a. HasRoomPosition a => a -> String -> Creep -> Effect Unit
moveToAndVisualize target strokeColor creep = 
  runEffectFn2 (unsafeCoerce creep).moveTo target { visualizePathStyle: {stroke: strokeColor} }

transfer :: ∀ a. a -> Resource -> Creep -> Effect StatusCode
transfer target payload creep =
  runEffectFn2 (unsafeCoerce creep).transfer target payload

upgradeController :: Controller -> Creep -> Effect StatusCode
upgradeController target creep = 
  runEffectFn1 (unsafeCoerce creep).upgradeController target

say :: String -> Creep -> Effect Unit
say msg creep = runEffectFn1 (unsafeCoerce creep).say msg

getMem :: ∀ a. DecodeJson a => Creep -> ExceptT JsonDecodeError Effect a
getMem creep = do 
  mem <- lift getRawMem
  except $ decodeJson mem

  where 
  getRawMem :: Effect Json
  getRawMem = pure (unsafeCoerce creep).memory

foreign import setMemImpl :: EffectFn2 Json Creep Unit
setMem :: ∀ a. EncodeJson a => a -> Creep -> Effect Unit
setMem mem creep = runEffectFn2 setMemImpl (encodeJson mem) creep

build :: ConstructionSite -> Creep -> Effect StatusCode
build target creep = runEffectFn1 (unsafeCoerce creep).build target

suicide :: Creep -> Effect Unit
suicide creep = (unsafeCoerce creep).suicide

name :: Creep -> String
name creep = (unsafeCoerce creep).name

