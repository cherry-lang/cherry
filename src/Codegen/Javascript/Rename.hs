{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Codegen.Javascript.Rename where

import           Control.Monad.State
import qualified Data.Map            as Map
import qualified Data.Set            as Set

import qualified Syntax              as Ch


type Name = String


data RenameState
  = RenameState
  { names   :: Set.Set Name
  , renamed :: Map.Map Name Name
  }


newtype Rename a = Rename { runRename :: State RenameState a }
  deriving (Functor, Applicative, Monad, MonadState RenameState)


emptyRenameState :: RenameState
emptyRenameState = RenameState Set.empty Map.empty


execRename :: Rename a -> RenameState
execRename m = execState (runRename m) emptyRenameState


evalRename :: Rename a -> RenameState -> a
evalRename m s = evalState (runRename m) s


rename :: Ch.Module -> Ch.Module
rename m = evalRename (rn m) $ execRename $ collect m


-- UTILS


mapInfixChar :: Char -> String
mapInfixChar c =
  case c of
    '|' -> "_pipe"
    '>' -> "_gt"
    '<' -> "_lt"
    '?' -> "_qm"
    '=' -> "_equals"
    '/' -> "_slash"
    '*' -> "_star"
    '^' -> "_hat"
    '$' -> "_dollar"
    '-' -> "_dash"
    '+' -> "_plus"
    '~' -> "_tilde"
    _   -> [c]


newName :: Name -> Rename Name
newName name = do
  let nn = foldl (++) [] $ map mapInfixChar name
  used <- gets names

  if name == nn
    then return name
    else if Set.member nn used
      then newName ("_" ++ name)
      else do
        modify $ \s -> s { renamed = Map.insert name nn $ renamed s }
        return nn


lookupRenamed :: Name -> Rename Name
lookupRenamed name = do
  rns <- gets renamed
  case Map.lookup name rns of
    Just name' ->
      return name'

    Nothing ->
      newName name


-- RENAME

class Renamable a where
  collect :: a -> Rename ()
  rn      :: a -> Rename a


instance Renamable Name where
  collect name = do
    modify $ \s -> s { names = name `Set.insert` names s }

  rn = lookupRenamed


instance Renamable Ch.Module where
  collect m =
    case m of
      Ch.Module { Ch.imports, Ch.exports, Ch.runs, Ch.decls } -> do
        mapM_ collect imports
        mapM_ collect exports
        mapM_ collect runs
        mapM_ collect decls

  rn m =
    case m of
      Ch.Module { Ch.imports, Ch.exports, Ch.runs, Ch.decls } -> do
        imports' <- mapM rn imports
        exports' <- mapM rn exports
        runs'    <- mapM rn runs
        decls'   <- mapM rn decls
        return $ m
          { Ch.exports = exports'
          , Ch.runs    = runs'
          , Ch.decls   = decls'
          , Ch.imports = imports'
          }


instance Renamable Ch.Import where
  collect imp =
    case imp of
      Ch.Import Ch.Cherry _ assigns ->
        mapM_ collect assigns

      _ ->
        return ()
  rn imp =
    case imp of
      Ch.Import Ch.Cherry name assigns -> do
        assigns' <- mapM rn assigns
        return $ Ch.Import Ch.Cherry name assigns'

      _ ->
        return imp


instance Renamable Ch.Declaration where
  collect decl =
    case decl of
      Ch.Func _ (name, _) params exprs -> do
        collect name
        mapM_ collect params
        mapM_ collect exprs

      Ch.Const _ name expr -> do
        collect name
        collect expr

      _ ->
        return ()

  rn decl =
    case decl of
      Ch.Func pos (name, arity) params exprs -> do
        name'   <- rn name
        params' <- mapM rn params
        exprs'  <- mapM rn exprs
        return $ Ch.Func pos (name', arity) params' exprs'

      Ch.Const pos name expr -> do
        name' <- rn name
        expr' <- rn expr
        return $ Ch.Const pos name' expr'

      _ ->
        return decl


instance Renamable Ch.Assign where
  collect import' =
    case import' of
      Ch.DefaultAs name ->
        collect name

      Ch.As name name' -> do
        collect name
        collect name'

      Ch.Plain name ->
        collect name

  rn import' =
    case import' of
      Ch.DefaultAs name -> do
        name' <- rn name
        return $ Ch.DefaultAs name'

      Ch.As name name' -> do
        name0 <- rn name
        name1 <- rn name'
        return $ Ch.As name0 name1

      Ch.Plain name -> do
        name' <- rn name
        return $ Ch.Plain name'


instance Renamable Ch.Expr where
  collect expr =
    case expr of
      Ch.Var _ name ->
        collect name

      Ch.Prop _ prop ->
        mapM_ collect prop

      Ch.App _ e1 e2 -> do
        collect e1
        collect e2

      Ch.Lambda params expr' -> do
        mapM_ collect params
        collect expr'

      _ ->
        return ()

  rn expr =
    case expr of
      Ch.Var pos name -> do
        name' <- rn name
        return $ Ch.Var pos name'

      Ch.Prop pos prop -> do
        prop' <- mapM rn prop
        return $ Ch.Prop pos prop'

      Ch.App pos e1 e2 -> do
        e1' <- rn e1
        e2' <- rn e2
        return $ Ch.App pos e1' e2'

      Ch.Lambda params expr' -> do
        params' <- mapM rn params
        expr0   <- rn expr'
        return $ Ch.Lambda params' expr0

      _ ->
        return expr
