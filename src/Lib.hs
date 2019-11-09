module Lib where
-- import Data.Int
import GHC.Generics
import Pinch


{-
enum Type {
  BOOLEAN = 0;
  INT32 = 1;
  INT64 = 2;
  INT96 = 3;  // deprecated, only used by legacy implementations.
  FLOAT = 4;
  DOUBLE = 5;
  BYTE_ARRAY = 6;
  FIXED_LEN_BYTE_ARRAY = 7;
}
-}

data PType
  = PBool (Enumeration 0)
  | PI32 (Enumeration 1)
  | PI64 (Enumeration 2)
  | PI96 (Enumeration 3)
  | PFloat (Enumeration 4)
  | PDouble (Enumeration 5)
  | PBArray (Enumeration 6)
  | PFixedBArray (Enumeration 7)
  deriving (Show, Ord, Eq, Generic)
  deriving anyclass (Pinchable)

-- instance Pinchable PType where
--   type Tag PType = TEnum
--   pinch PBool = pinch (0 :: Int32)
--   pinch PI32 = pinch (1 :: Int32)
--   pinch PI64 = pinch (2 :: Int32)
--   pinch PI96 = pinch (3 :: Int32)
--   pinch PFloat = pinch (4 :: Int32)
--   pinch PDouble = pinch (5 :: Int32)
--   pinch PBArray = pinch (6 :: Int32)
--   pinch PFixedBArray = pinch (7 :: Int32)
--   unpinch v = do
--     value <- unpinch v
--     case (value :: Int32) of
--       0 -> Right PBool
--       1 -> Right PI32
--       2 -> Right PI64
--       3 -> Right PI96
--       4 -> Right PFloat
--       5 -> Right PDouble
--       6 -> Right PBArray
--       7 -> Right PFixedBArray
--       _ -> Left $ "Unknown PType: " ++ show value
