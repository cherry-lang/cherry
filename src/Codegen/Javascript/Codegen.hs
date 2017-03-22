{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Codegen.Javascript.Codegen where

import           Control.Monad.State
import           Data.Map                  ((!))
import qualified Data.Map                  as Map

import qualified Codegen.Javascript.Syntax as Js
import qualified Syntax                    as Ch
import           Utils


type Name = String
type Arity = Int


data CodegenState
  = CodegenState
  { blocks  :: Map.Map Name Block
  , current :: Name
  }


data Block
  = Func (Map.Map Arity Js.Statement)
  | Const Name Js.Expr
  | Void


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)


codegen :: Ch.Module -> Js.Module
codegen = evalCodegenBlocks . execCodegen . module'


emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState Map.empty ""


execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegenState


evalCodegenBlocks :: CodegenState -> Js.Module
evalCodegenBlocks (CodegenState { blocks }) =
  blocks
    |> Map.toList
    |> map blockToJs
    |> Js.Module


currentBlock :: Codegen Block
currentBlock = do
  current' <- gets current
  blocks'  <- gets blocks
  return $ Map.findWithDefault Void current' blocks'


putBlock :: Name -> Block -> Codegen ()
putBlock name block =
  modify $ \s -> s
    { blocks  = Map.insert name block $ blocks s
    , current = name
    }


putFunc :: Name -> Arity -> Js.Statement -> Codegen ()
putFunc name arity func = do
  blocks' <- gets blocks

  case Map.lookup name blocks' of
    Nothing ->
      putBlock name $ Func $ Map.singleton arity func

    Just (Func impl) ->
      putBlock name $ Func $ Map.insert arity func impl


putStatement :: Arity -> Js.Statement -> Codegen ()
putStatement arity statement = do
  (Func impl) <- currentBlock

  let (Js.Func name params statements) = impl ! arity
  let block = Func $ Map.insert arity (Js.Func name params $ statements ++ [statement]) impl

  putBlock name block


-- PARTS


blockToJs :: (Name, Block) -> Js.Statement
blockToJs (name, block) =
  case block of
    Func impl -> Js.Func name [] $
      impl
        |> Map.toList
        |> map (\(arity, func) -> (arityComparison arity, [Js.Return $ funcToLambda func]))
        |> (\x -> [Js.If x])

    Const name' val ->
      Js.Const name' val

    Void ->
      undefined


arityComparison :: Int -> Js.Expr
arityComparison arity =
  Js.Comp Js.Eq
    (Js.Var $ "arguments[" ++ show arity ++ "]")
    (Js.Lit $ Js.Number $ fromIntegral arity)


funcToLambda :: Js.Statement -> Js.Expr
funcToLambda (Js.Func _ params statements) = Js.Lambda params statements
funcToLambda _                             = undefined


-- GENERATION


module' :: Ch.Module -> Codegen ()
module' (Ch.Module _ _ _ decls) = void $ mapM decl decls


decl :: Ch.Declaration -> Codegen ()
decl d =
  case d of
    Ch.Func _ (name, arity) params exprs -> do
      putFunc name arity $ Js.Func name params []


expr :: Ch.Expr -> Codegen ()
expr e =
  case e of
    Ch.Var _ name ->
      return ()
