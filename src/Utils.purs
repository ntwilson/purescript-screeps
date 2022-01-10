module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Exception (Error, error)

liftError :: ∀ e a m. Show e => MonadThrow Error m => Either e a -> m a
liftError (Right a) = pure a
liftError (Left e) = throwError $ error $ show e

orThrow :: ∀ e m a. MonadThrow e m => e -> Maybe a -> m a
orThrow _ (Just x) = pure x
orThrow err Nothing = throwError err

orThrowError :: ∀ m a. MonadThrow Error m => String -> Maybe a -> m a
orThrowError _ (Just x) = pure x
orThrowError msg Nothing = throwError $ error msg

onWrapped :: ∀ t a b. Newtype t a => t -> (a -> b) -> b
onWrapped x fn = fn $ unwrap x 

infixl 1 onWrapped as ##
