{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Flooring.Internal.Generics where
import Data.Aeson
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Pinch (Enumeration)

newtype JSONEnumeration a = JSONEnumeration a

class GEnumeration f where
  enumToJSON :: f p -> Value

instance (Generic a, GEnumeration (Rep a)) => ToJSON (JSONEnumeration a) where
  toJSON (JSONEnumeration a) = enumToJSON (from a)

instance (GEnumeration f, GEnumeration g) => GEnumeration (f :+: g) where
  enumToJSON (L1 x) = enumToJSON x
  enumToJSON (R1 x) = enumToJSON x

instance KnownSymbol a => GEnumeration (M1 C ('MetaCons a b c) (S1 d (Rec0 (Enumeration e)))) where
  enumToJSON _ = toJSON (symbolVal (Proxy :: Proxy a))

instance GEnumeration b => GEnumeration (M1 D a b) where
  enumToJSON (M1 x) = enumToJSON x
