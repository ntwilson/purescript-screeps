module Utils where

import Screeps.Prelude

import Data.Lazy (Lazy, force)

orThrow :: ∀ e m a. MonadThrow e m => e -> Maybe a -> m a
orThrow _ (Just x) = pure x
orThrow err Nothing = throwError err

orThrowError :: ∀ m a. MonadThrow Error m => String -> Maybe a -> m a
orThrowError _ (Just x) = pure x
orThrowError msg Nothing = throwError $ error msg

noteL :: ∀ a b. (Lazy a) -> Maybe b -> Either a b
noteL _ (Just b) = Right b
noteL a Nothing = Left $ force a
