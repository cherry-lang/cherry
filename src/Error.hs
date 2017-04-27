{-# LANGUAGE DeriveGeneric #-}

module Error (toFriendlyParseError, toFriendlyTypeError, jsonErr) where

import           GHC.Generics

import           Data.Aeson
import           Data.List
import qualified Data.List.NonEmpty as Ne
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Text.Megaparsec    as P

import qualified Parse              as P
import qualified Syntax.Position    as Syn
import qualified Type               as T
import qualified Typecheck.Error    as T
import           Utils


data ErrorContext = ErrorContext
  { file    :: FilePath
  , line    :: Int
  , col     :: Int
  , srcLine :: String
  }
  deriving (Generic, Show)


data Error
  = UnboundError
  { context :: ErrorContext
  , got     :: String
  }

  | UndefinedError
  { context :: ErrorContext
  , got     :: String
  }

  | TypeMismatch
  { context  :: ErrorContext
  , got      :: String
  , expected :: String
  }

  | ParseError
  { context  :: ErrorContext
  , got      :: String
  , expected :: String
  }

  deriving (Generic, Show)


instance ToJSON ErrorContext where
  toEncoding = genericToEncoding defaultOptions


instance ToJSON Error where
  toEncoding = genericToEncoding defaultOptions


emptyContext :: ErrorContext
emptyContext =
  ErrorContext
    { file = ""
    , line = 0
    , col  = 0
    , srcLine = ""
    }


posToContext :: Syn.Pos -> IO ErrorContext
posToContext (Syn.Pos fp ln col) =
  readFile fp >>= \src ->
    return $ ErrorContext
      { file    = fp
      , line    = ln
      , col     = col
      , srcLine = (lines src) !! (ln - 1)
      }


typeToString :: T.Type -> String
typeToString t =
  case t of
    T.Var (T.TV x) ->
      x

    T.Term x vars ->
      foldl (++) "" (x:(Set.toList $ Set.map typeToString vars))

    T.Arrow x x' ->
      (typeToString x) ++ " -> " ++ (typeToString x')

    T.Record props ->
      props
        |> Map.toList
        |> map (\(k, v) -> k ++ " :: " ++ (typeToString v))
        |> intersperse ", "
        |> foldl (++) ""
        |> \r -> "{ " ++ r ++ " }"


jsonErr :: Error -> String
jsonErr = show . encode


toFriendlyTypeError :: T.Error -> IO Error
toFriendlyTypeError err =
  case err of
    T.TypeMismatch pos g e -> do
      context <- posToContext pos
      return $ TypeMismatch
        { context  = context
        , got      = typeToString g
        , expected = typeToString e
        }

    T.UnboundVariable pos var -> do
      context <- posToContext pos
      return $ UnboundError
        { context = context
        , got     = var
        }

    T.UnboundProperty pos rec var -> do
      context <- posToContext pos
      return $ UnboundError
        { context = context
        , got     = rec ++ "." ++ var
        }

    T.UndefinedType pos (T.Term t _) -> do
      context <- posToContext pos
      return $ UndefinedError
        { context = context
        , got     = t
        }

    _ ->
      return $ UnboundError
      { context = emptyContext
      , got     = "Ops not sure about this. " ++ (show err)
      }


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
