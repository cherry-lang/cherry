{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Codegen.Javascript.Transform where

import qualified Codegen.Javascript.Syntax as Js
import qualified Syntax                    as Ch
import Utils


codegen :: Ch.Module -> Js.Module
codegen = transform


type Name = String


traverseApp :: Ch.Expr -> [Ch.Expr]
traverseApp (Ch.App _ e1 e2) = (traverseApp e1) ++ [e2]
traverseApp expr             = [expr]


class Transform a b where
  transform :: a -> b


instance Transform Ch.Module Js.Module where
  transform m@(Ch.Module { Ch.imports, Ch.exports, Ch.runs, Ch.decls }) =
     Js.Module $
       map (resolveImports m) imports
       ++ map transform decls
       ++ map (Js.Expr . transform) runs


resolveImports :: Ch.Module -> Ch.Import -> Js.Statement
resolveImports (Ch.Module { Ch.name }) (Ch.Import _ name' imports) =
  Js.Import (resolveImportPath name name') $ map transform imports


instance Transform Ch.Assign String where
  transform (Ch.DefaultAs name) = "default as " ++ name
  transform (Ch.As name name')  = name ++ " as " ++ name'
  transform (Ch.Plain name)     = name


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
