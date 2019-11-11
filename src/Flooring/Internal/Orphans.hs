{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flooring.Internal.Orphans where
import Data.Aeson (ToJSON(..))
import Data.ByteString
import Data.ByteString.Base64 (encode)
import Data.Text.Encoding (decodeUtf8)
import Pinch (Field(..))

-- TODO: remove these

instance ToJSON a => ToJSON (Field n a) where
  toJSON (Field a) = toJSON a

instance ToJSON ByteString where
  toJSON bs = toJSON . decodeUtf8 $ encode bs
