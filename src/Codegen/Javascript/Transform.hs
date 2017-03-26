{-# LANGUAGE MultiParamTypeClasses      #-}

module Codegen.Javascript.Transform where

import qualified Codegen.Javascript.Syntax as Js
import qualified Syntax                    as Ch


codegen :: Ch.Module -> Js.Module
codegen = transform


type Name = String


traverseApp :: Ch.Expr -> [Ch.Expr]
traverseApp (Ch.App _ e1 e2) = (traverseApp e1) ++ [e2]
traverseApp expr             = [expr]


class Transform a b where
  transform :: a -> b


instance Transform Ch.Module Js.Module where
  transform (Ch.Module _ exports runs decls) =
     Js.Module $
       map transform decls
       ++ map (Js.Expr . transform) runs


instance Transform Ch.Declaration Js.Statement where
  transform decl =
    case decl of
      Ch.Func _ (name, _) params exprs ->
        let
          statements = map (Js.Expr . transform) (init exprs) ++
                       [Js.Return $ transform $ last exprs]
        in
          Js.Func name params statements

      Ch.TypeAnn _ _ _ ->
        Js.Skip

      Ch.ImportJs _ src imports' ->
        Js.Import src $ map (\(Ch.Plain x) -> x) imports'


instance Transform Ch.Expr Js.Expr where
  transform expr =
    case expr of
      Ch.Lit _ lit ->
        Js.Lit $ transform lit

      Ch.Var _ name ->
        Js.Var name

      Ch.Prop _ prop ->
        Js.Prop prop

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
        Js.Void
