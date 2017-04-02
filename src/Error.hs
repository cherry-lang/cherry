{-# LANGUAGE DeriveGeneric #-}

module Error (toFriendlyParseError, jsonErr) where

import GHC.Generics

import Data.Aeson
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as Ne
import qualified Text.Megaparsec    as P

import qualified Parse              as P


data ErrorContext = ErrorContext
  { file    :: FilePath
  , line    :: Int
  , col     :: Int
  , srcLine :: String
  }
  deriving (Generic, Show)


data Error
  = TypeError { context :: ErrorContext, got :: String, expected :: String }
  | ParseError { context :: ErrorContext, got :: String, expected :: String }
  deriving (Generic, Show)


instance ToJSON ErrorContext where
  toEncoding = genericToEncoding defaultOptions


instance ToJSON Error where
  toEncoding = genericToEncoding defaultOptions


jsonErr :: Error -> String
jsonErr = show . encode


toFriendlyParseError :: P.ParseErr -> IO Error
toFriendlyParseError err =
  let
    (P.SourcePos fp ln col') = Ne.head $ P.errorPos err

    line' = fromIntegral $ P.unPos ln
  in
    readFile fp >>= \src ->
      return ParseError
        { context = ErrorContext
          { file = fp
          , line = line'
          , col  = fromIntegral $ P.unPos col'
          , srcLine = (lines src) !! (line' - 1)
          }
        , got      = show $ head $ Set.toList $ P.errorUnexpected err
        , expected = show $ head $ Set.toList $ P.errorExpected err
        }
