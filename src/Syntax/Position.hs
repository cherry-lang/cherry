module Syntax.Position where


data Pos
  = Pos
  { srcFile :: FilePath
  , srcLine :: Int
  , srcCol  :: Int
  }
  deriving (Show)
