module Flooring.Internal.Orphans where
import Data.Aeson
import Pinch

instance ToJSON a => ToJSON (Field n a) where
  toJSON (Field a) = toJSON a
