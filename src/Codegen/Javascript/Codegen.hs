{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Codegen.Javascript.Codegen where

import           Control.Monad.State
import           Data.Map                  ((!))
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set

import qualified Codegen.Javascript.Syntax as Js
import qualified Syntax                    as Ch
import           Utils


codegen :: Ch.Module -> Js.Module
codegen = transform


-- TYPES


type Name = String


-- TRANSFORM


traverseApp :: Ch.Expr -> [Ch.Expr]
traverseApp (Ch.App _ e1 e2) = (traverseApp e1) ++ [e2]
traverseApp expr             = [expr]


class Transform a b where
  transform :: a -> b


instance Transform Ch.Module Js.Module where
  transform (Ch.Module _ exports runs decls) =
     Js.Module $ map transform decls


instance Transform Ch.Declaration Js.Statement where
  transform decl =
    case decl of
      Ch.Func _ (name, _) params exprs ->
        let
          statements = map (Js.Expr . transform) (tail exprs) ++
                       [Js.Return $ transform $ head exprs]
        in
          Js.Func name params statements


instance Transform Ch.Expr Js.Expr where
  transform expr =
    case expr of
      Ch.Lit _ lit ->
        Js.Lit $ transform lit

      Ch.Var _ name ->
        Js.Var name

      Ch.App _ e1 e2 ->
        let
          exprs = traverseApp e1 ++ [e2]
        in
          Js.App (transform $ head exprs) $ map transform $ tail exprs

      Ch.Lambda params expr' ->
        Js.Lambda params [Js.Expr $ transform expr']


instance Transform Ch.Lit Js.Lit where
  transform lit =
    case lit of
      Ch.Bool bool ->
        Js.Bool bool

      Ch.String str ->
        Js.String str

      Ch.Int int ->
        Js.Number $ fromIntegral int

      Ch.Float float ->
        Js.Number float

      Ch.Void ->
        undefined
