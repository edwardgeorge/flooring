module Lib where
import Prelude hiding (drop)
import Control.Exception
import Control.Monad
import Data.Int
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize.Get
import Pinch
import Pinch.Protocol
import System.IO

import Flooring.Schema

data MissingParquetMagicNumber = MissingParquetMagicNumber
  deriving (Eq, Ord, Show)

instance Exception MissingParquetMagicNumber

getFooterLength :: Handle -> IO Int32
getFooterLength h = do
  hSeek h SeekFromEnd (-8)
  bs <- BS.hGet h 8
  when (BS.drop 4 bs /= "PAR1") $ throwIO MissingParquetMagicNumber
  case runGet getInt32le (BS.take 4 bs) of
    Right v -> return v
    Left err -> error err -- TODO: throwIO

getFooterBS :: Handle -> IO ByteString
getFooterBS h = do
  len <- getFooterLength h
  hSeek h SeekFromEnd (- (fromIntegral len + 8))
  BS.hGet h $ fromIntegral len

getStruct :: Handle -> IO (Value TStruct)
getStruct h = do
  bs <- getFooterBS h
  case deserializeValue compactProtocol bs of
    Left err -> error err -- TODO: throwIO
    Right v -> return v

getFileMetadata :: Handle -> IO FileMetadata
getFileMetadata h = do
  val <- getStruct h
  case runParser (unpinch val) of
    Left err -> error err -- TODO: throwIO
    Right v -> return v
