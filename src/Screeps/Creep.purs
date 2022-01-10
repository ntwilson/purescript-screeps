module Screeps.Creep
  ( Creep(..)
  , Payload
  , StatusCode
  , build
  , err_not_in_range
  , getMem
  , harvest
  , moveTo
  , moveToAndVisualize
  , resource_energy
  , room
  , say
  , setMem
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
import Screeps.ConstructionSite (ConstructionSite)
import Screeps.Controller (Controller)
import Screeps.Room (Room)
import Screeps.RoomPosition (class HasRoomPosition)
import Screeps.Source (Source)
import Screeps.Store (class HasStore)
import Unsafe.Coerce (unsafeCoerce)

foreign import data StatusCode :: Type
instance Eq StatusCode where
  eq = runFn2 statusCodeEq
foreign import err_not_in_range :: StatusCode
foreign import statusCodeEq :: Fn2 StatusCode StatusCode Boolean

foreign import data Payload :: Type
instance Eq Payload where
  eq = runFn2 payloadEq
foreign import payloadEq :: Fn2 Payload Payload Boolean
foreign import resource_energy :: Payload

foreign import data Creep :: Type
instance HasRoomPosition Creep
instance HasStore Creep where store s = (unsafeCoerce s).store

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

transfer :: ∀ a. a -> Payload -> Creep -> Effect StatusCode
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
