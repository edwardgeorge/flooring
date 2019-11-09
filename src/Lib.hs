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


{-
enum ConvertedType {
  /** a BYTE_ARRAY actually contains UTF8 encoded chars */
  UTF8 = 0;

  /** a map is converted as an optional field containing a repeated key/value pair */
  MAP = 1;

  /** a key/value pair is converted into a group of two fields */
  MAP_KEY_VALUE = 2;

  /** a list is converted into an optional field containing a repeated field for its
   * values */
  LIST = 3;

  /** an enum is converted into a binary field */
  ENUM = 4;

  /**
   * A decimal value.
   *
   * This may be used to annotate binary or fixed primitive types. The
   * underlying byte array stores the unscaled value encoded as two's
   * complement using big-endian byte order (the most significant byte is the
   * zeroth element). The value of the decimal is the value * 10^{-scale}.
   *
   * This must be accompanied by a (maximum) precision and a scale in the
   * SchemaElement. The precision specifies the number of digits in the decimal
   * and the scale stores the location of the decimal point. For example 1.23
   * would have precision 3 (3 total digits) and scale 2 (the decimal point is
   * 2 digits over).
   */
  DECIMAL = 5;

  /**
   * A Date
   *
   * Stored as days since Unix epoch, encoded as the INT32 physical type.
   *
   */
  DATE = 6;

  /**
   * A time
   *
   * The total number of milliseconds since midnight.  The value is stored
   * as an INT32 physical type.
   */
  TIME_MILLIS = 7;

  /**
   * A time.
   *
   * The total number of microseconds since midnight.  The value is stored as
   * an INT64 physical type.
   */
  TIME_MICROS = 8;

  /**
   * A date/time combination
   *
   * Date and time recorded as milliseconds since the Unix epoch.  Recorded as
   * a physical type of INT64.
   */
  TIMESTAMP_MILLIS = 9;

  /**
   * A date/time combination
   *
   * Date and time recorded as microseconds since the Unix epoch.  The value is
   * stored as an INT64 physical type.
   */
  TIMESTAMP_MICROS = 10;


  /**
   * An unsigned integer value.
   *
   * The number describes the maximum number of meaningful data bits in
   * the stored value. 8, 16 and 32 bit values are stored using the
   * INT32 physical type.  64 bit values are stored using the INT64
   * physical type.
   *
   */
  UINT_8 = 11;
  UINT_16 = 12;
  UINT_32 = 13;
  UINT_64 = 14;

  /**
   * A signed integer value.
   *
   * The number describes the maximum number of meaningful data bits in
   * the stored value. 8, 16 and 32 bit values are stored using the
   * INT32 physical type.  64 bit values are stored using the INT64
   * physical type.
   *
   */
  INT_8 = 15;
  INT_16 = 16;
  INT_32 = 17;
  INT_64 = 18;

  /**
   * An embedded JSON document
   *
   * A JSON document embedded within a single UTF8 column.
   */
  JSON = 19;

  /**
   * An embedded BSON document
   *
   * A BSON document embedded within a single BINARY column.
   */
  BSON = 20;

  /**
   * An interval of time
   *
   * This type annotates data stored as a FIXED_LEN_BYTE_ARRAY of length 12
   * This data is composed of three separate little endian unsigned
   * integers.  Each stores a component of a duration of time.  The first
   * integer identifies the number of months associated with the duration,
   * the second identifies the number of days associated with the duration
   * and the third identifies the number of milliseconds associated with
   * the provided duration.  This duration of time is independent of any
   * particular timezone or date.
   */
  INTERVAL = 21;
}
-}-}

data ConvertedType
  = PUTF8 (Enumeration 0)
  | PMap (Enumeration 1)
  | PMapKV (Enumeration 2)
  | PList (Enumeration 3)
  | PEnum (Enumeration 4)
  | PDecimal (Enumeration 5)
  | PDate (Enumeration 6)
  | PTimeMillis (Enumeration 7)
  | PTimeMicros (Enumeration 8)
  | PTimestampMillis (Enumeration 9)
  | PTimestampMicros (Enumeration 10)
  | PUInt8 (Enumeration 11)
  | PUInt16 (Enumeration 12)
  | PUInt32 (Enumeration 13)
  | PUint64 (Enumeration 14)
  | PInt8 (Enumeration 15)
  | PInt16 (Enumeration 16)
  | PInt32 (Enumeration 17)
  | PInt64 (Enumeration 18)
  | PJSON (Enumeration 19)
  | PBSON (Enumeration 20)
  | PInterval (Enumeration 21)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)
