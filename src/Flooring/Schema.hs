module Flooring.Schema where
import Data.Aeson (ToJSON(..))
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Pinch

import Flooring.Internal.Generics
import Flooring.Internal.Orphans ()

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
  deriving ToJSON via (JSONEnumeration PType)

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
  deriving ToJSON via (JSONEnumeration ConvertedType)

data FieldRepetitionType
  = Required (Enumeration 0)
  | Optional (Enumeration 1)
  | Repeated (Enumeration 2)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)
  deriving ToJSON via (JSONEnumeration FieldRepetitionType)

data Statistics
  = Statistics
  { stMax :: Field 1 (Maybe ByteString)
  , stMin :: Field 2 (Maybe ByteString)
  , stNullCount :: Field 3 (Maybe Int64)
  , stDistinctCount :: Field 4 (Maybe Int64)
  , stMaxValue :: Field 5 (Maybe ByteString)
  , stMinValue :: Field 6 (Maybe ByteString)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data EmptyStruct = EmptyStruct
  deriving (Eq, Ord, Show)

instance Pinchable EmptyStruct where
  type Tag EmptyStruct = TStruct
  pinch EmptyStruct = struct []
  unpinch _ = pure EmptyStruct

data DecimalType
  = DecimalType
  { decScale :: Field 1 Int32
  , decPrecision :: Field 2 Int32
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data TimeUnit
  = TUMillis (Field 1 EmptyStruct)
  | TUMicros (Field 2 EmptyStruct)
  | TUNanos (Field 3 EmptyStruct)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data TimestampType
  = TimestampType
  { tsIsAdjustedToUTC :: Field 1 Bool
  , tsUnit :: Field 2 TimeUnit
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data TimeType
  = TimeType
  { tIsAdjustedToUTC :: Field 1 Bool
  , tUnit :: Field 2 TimeUnit
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data IntType
  = IntType
  { bitWidth :: Field 1 Int8
  , isSigned :: Field 2 Bool
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data LogicalType
  = LStringType (Field 1 EmptyStruct)
  | LMapType (Field 2 EmptyStruct)
  | LListType (Field 3 EmptyStruct)
  | LEnumType (Field 4 EmptyStruct)
  | LDecimalType (Field 5 DecimalType)
  | LDateType (Field 6 EmptyStruct)
  | LTimeType (Field 7 TimeType)
  | LTimestampType (Field 8 TimestampType)
  | LIntType (Field 9 IntType)
  | LNullType (Field 10 EmptyStruct)
  | LJsonType (Field 11 EmptyStruct)
  | LBsonType (Field 12 EmptyStruct)
  | LUUIDType (Field 13 EmptyStruct)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data SchemaElement
  = SchemaElement
  { schType :: Field 1 (Maybe PType)
  , schTypeLength :: Field 2 (Maybe Int32)
  , schRepetitionType :: Field 3 (Maybe FieldRepetitionType)
  , schName :: Field 4 Text
  , schNumChildren :: Field 5 (Maybe Int32)
  , schConvertedType :: Field 6 (Maybe ConvertedType)
  , schScale :: Field 7 (Maybe Int32)
  , schPrecision :: Field 8 (Maybe Int32)
  , schFieldId :: Field 9 (Maybe Int32)
  , schLogicalType :: Field 10 (Maybe LogicalType)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data Encoding
  = Plain (Enumeration 0)
  | PlainDictionary (Enumeration 2)
  | RLE (Enumeration 3)
  | BitPacked (Enumeration 4)
  | DeltaBinaryPacked (Enumeration 5)
  | DeltaLengthByteArray (Enumeration 6)
  | DeltaByteArray (Enumeration 7)
  | RLEDictionary (Enumeration 8)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data CompressionCodec
  = Uncompressed (Enumeration 0)
  | Snappy (Enumeration 1)
  | GZip (Enumeration 2)
  | LZO (Enumeration 3)
  | Brotli (Enumeration 4)
  | LZ4 (Enumeration 5)
  | ZSTD (Enumeration 6)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data PageType
  = DataPage (Enumeration 0)
  | IndexPage (Enumeration 1)
  | DictionaryPage (Enumeration 2)
  | DataPageV2 (Enumeration 3)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data BoundaryOrder
  = Unordered (Enumeration 0)
  | Ascending (Enumeration 1)
  | Descending (Enumeration 2)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data DataPageHeader
  = DataPageHeader
  { dphNumValues :: Field 1 Int32
  , dphEncoding :: Field 2 Encoding
  , dphDefinitionLevelEncoding :: Field 3 Encoding
  , dphRepetitionLevelEncoding :: Field 4 Encoding
  , dphStatistics :: Field 5 (Maybe Statistics)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

type IndexPageHeader = EmptyStruct

data DictionaryPageHeader
  = DictionaryPageHeader
  { dicNumValues :: Field 1 Int32
  , dicEncoding :: Field 2 Encoding
  , dicIsSorted :: Field 3 (Maybe Bool)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data DataPageHeaderV2
  = DataPageHeaderV2
  { dph2NumValues :: Field 1 Int32
  , dph2NumNulls :: Field 2 Int32
  , dph2NumRows :: Field 3 Int32
  , dph2Encoding :: Field 4 Encoding
  , dph2DefinitionLevelsByteLength :: Field 5 Int32
  , dph2RepetitionLevelsByteLength :: Field 6 Int32
  , dph2IsCompressed :: Field 7 (Maybe Bool)
  , dph2Statistics :: Field 8 (Maybe Statistics)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

type SplitBlockAlgorithm = EmptyStruct

data BloomFilterAlgorithm
  = BFABlock SplitBlockAlgorithm
  deriving (Eq, Ord, Show, Generic)

-- manually implement union with only one branch
instance Pinchable BloomFilterAlgorithm where
  type Tag BloomFilterAlgorithm = TUnion
  pinch (BFABlock f) = union 1 f
  unpinch v = BFABlock <$> v .: 1

type XxHash = EmptyStruct

data BloomFilterHash
  = BFHXxHash XxHash
  deriving (Eq, Ord, Show, Generic)

-- manually implement union with only one branch
instance Pinchable BloomFilterHash where
  type Tag BloomFilterHash = TUnion
  pinch (BFHXxHash h) = union 1 h
  unpinch v = BFHXxHash <$> v .: 1

type Uncompressed = EmptyStruct

data BloomFilterCompression
  = BFCUncompressed Uncompressed
  deriving (Eq, Ord, Show, Generic)

instance Pinchable BloomFilterCompression where
  type Tag BloomFilterCompression = TUnion
  pinch (BFCUncompressed u) = union 1 u
  unpinch v = BFCUncompressed <$> v .: 1

data BloomFilterHeader
  = BloomFilterHeader
  { bfhNumBytes :: Field 1 Int32
  , bfhAlgorithm :: Field 2 BloomFilterAlgorithm
  , bfhHash :: Field 3 BloomFilterHash
  , bfhCompression :: Field 4 BloomFilterCompression
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data PageHeader
  = PageHeader
  { phType :: Field 1 PageType
  , phUncompressedPageSize :: Field 2 Int32
  , phCompressedPageSize :: Field 3 Int32
  , phCRC :: Field 4 (Maybe Int32)
  , phDataPageHeader :: Field 5 (Maybe DataPageHeader)
  , phIndexPageHeader :: Field 6 (Maybe IndexPageHeader)
  , phDictionaryPageHeader :: Field 7 (Maybe DictionaryPageHeader)
  , phDataPageHeaderV2 :: Field 8 (Maybe DataPageHeaderV2)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data KeyValue
  = KeyValue
  { kvKey :: Field 1 Text
  , kvValue :: Field 2 (Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data SortingColumn
  = SortingColumn
  { scColumnIdx :: Field 1 Int32
  , scDescending :: Field 2 Bool
  , scNullsFirst :: Field 3 Bool
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data PageEncodingStats
  = PageEncodingStats
  { pesPageType :: Field 1 PageType
  , pesEncoding :: Field 2 Encoding
  , pesCount :: Field 3 Int32
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data ColumnMetadata
  = ColumnMetadata
  { colType :: Field 1 PType
  , colEncodings :: Field 2 (Vector Encoding)
  , colPathInSchema :: Field 3 (Vector Text)
  , colCodec :: Field 4 CompressionCodec
  , colNumValues :: Field 5 Int64
  , colTotalUncompressedSize :: Field 6 Int64
  , colTotalCompressedSize :: Field 7 Int64
  , colKeyValueMetadata :: Field 8 (Maybe (Vector KeyValue))
  , colDataPageOffset :: Field 9 Int64
  , colIndexPageOffset :: Field 10 (Maybe Int64)
  , colDictionaryPageOffset :: Field 11 (Maybe Int64)
  , colStatistics :: Field 12 (Maybe Statistics)
  , colEncodingStats :: Field 13 (Maybe (Vector PageEncodingStats))
  , colBloomFilterOffset :: Field 14 (Maybe Int64)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

type EncryptionWithFooterKey = EmptyStruct

data EncryptionWithColumnKey
  = EncryptionWithColumnKey
  { eckPathInSchema :: Field 1 (Vector Text)
  , eckKeyMetadata :: Field 2 (Maybe ByteString)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data ColumnCryptoMetadata
  = ColEncryptionFK (Field 1 EncryptionWithFooterKey)
  | ColEncryptionCK (Field 2 EncryptionWithColumnKey)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data ColumnChunk
  = ColumnChunk
  { cchFilePath :: Field 1 (Maybe Text)
  , cchFileOffset :: Field 2 Int64
  , cchMetadata :: Field 3 (Maybe ColumnMetadata)
  , cchOffsetIndexOffset :: Field 4 (Maybe Int64)
  , cchOffsetIndexLength :: Field 5 (Maybe Int32)
  , cchColumnIndexOffset :: Field 6 (Maybe Int64)
  , cchColumnIndexLength :: Field 7 (Maybe Int32)
  , cchCryptoMetadata :: Field 8 (Maybe ColumnCryptoMetadata)
  , cchEncryptedColumnMetadata :: Field 9 (Maybe ByteString)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data RowGroup
  = RowGroup
  { rgrpColumns :: Field 1 (Vector ColumnChunk)
  , rgrpTotalByteSize :: Field 2 Int64
  , rgrpNumRows :: Field 3 Int64
  , rgrpSortingColumns :: Field 4 (Maybe (Vector SortingColumn))
  , rgrpFileOffset :: Field 5 (Maybe Int64)
  , rgrpTotalCompressedSize :: Field 6 (Maybe Int64)
  , rgrpOrdinal :: Field 7 (Maybe Int16)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

type TypeDefinedOrder = EmptyStruct

data ColumnOrder
  = TypeOrder TypeDefinedOrder
  deriving (Eq, Ord, Show, Generic)

-- manually implement union with only one branch
instance Pinchable ColumnOrder where
  type Tag ColumnOrder = TUnion
  pinch (TypeOrder o) = union 1 o
  unpinch v = TypeOrder <$> v .: 1

data PageLocation
  = PageLocation
  { plocOffset :: Field 1 Int64
  , plocCompressedPageSize :: Field 2 Int32
  , plocFirstRowIndex :: Field 3 Int64
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data OffsetIndex
  = OffsetIndex
  { pageLocations :: Field 1 (Vector PageLocation)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data ColumnIndex
  = ColumnIndex
  { cixNullPages :: Field 1 (Vector Bool)
  , cixMinValues :: Field 2 (Vector ByteString)
  , cixMaxValues :: Field 3 (Vector ByteString)
  , cixBoundaryOrder :: Field 4 BoundaryOrder
  , cixNullCounts :: Field 5 (Maybe (Vector Int64))
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data AesGcmCtrV1
  = AesGcmCtrV1
  { aadPrefix :: Field 1 (Maybe ByteString)
  , aadFileUnique :: Field 2 (Maybe ByteString)
  , supplyAadPrefix :: Field 3 (Maybe Bool)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

type AesGcmV1 = AesGcmCtrV1

data EncryptionAlgorithm
  = EAAESGCMv1 (Field 1 AesGcmV1)
  | EAAESGCMCTRv1 (Field 2 AesGcmCtrV1)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data FileMetadata
  = FileMetadata
  { fmVersion :: Field 1 Int32
  , fmSchema :: Field 2 (Vector SchemaElement)
  , fmNumRows :: Field 3 Int64
  , fmRowGroups :: Field 4 (Vector RowGroup)
  , fmKeyValueMetadata :: Field 5 (Maybe (Vector KeyValue))
  , fmCreatedBy :: Field 6 (Maybe Text)
  , fmColumnOrders :: Field 7 (Maybe (Vector ColumnOrder))
  , fmEncryptionAlgorithm :: Field 8 (Maybe EncryptionAlgorithm)
  , fmFooterSigningKeyMetadata :: Field 9 (Maybe ByteString)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)

data FileCryptoMetadata
  = FileCryptoMetadata
  { fcmEncryptionAlgorithm :: Field 1 EncryptionAlgorithm
  , fcmKeyMetadata :: Field 2 (Maybe ByteString)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Pinchable)
