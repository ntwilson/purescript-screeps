module FFI.Hits where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

class HasHits :: Type -> Constraint
class HasHits a

hits :: ∀ a. HasHits a => a -> Int
hits it = (unsafeCoerce it).hits

hitsMax :: ∀ a. HasHits a => a -> Int
hitsMax it = (unsafeCoerce it).hitsMax

hitsPercent :: ∀ a. HasHits a => a -> Maybe Number
hitsPercent it 
  | hitsMax it == 0 = Nothing
  | otherwise = Just (Int.toNumber (hits it) / Int.toNumber (hitsMax it))

