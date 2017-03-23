{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.Javascript.Merge where

import           Control.Monad.State
import           Data.List                 (find, deleteBy)
import qualified Data.Map                  as Map

import qualified Codegen.Javascript.Syntax as Js


type Name = String
type Arity = Int


data MergeState
  = MergeState
  { blocks :: [Block]
  }


data Block
  = Func Name (Map.Map Arity Js.Statement)
  | Other Js.Statement


newtype Merge a = Merge { runMerge :: State MergeState a }
  deriving (Functor, Applicative, Monad, MonadState MergeState)


merge :: Js.Module -> Js.Module
merge = execMerge . module'


emptyMergeState :: MergeState
emptyMergeState = MergeState []


execMerge :: Merge a -> Js.Module
execMerge m = mergeStateToModule $ execState (runMerge m) emptyMergeState


mergeStateToModule :: MergeState -> Js.Module
mergeStateToModule = Js.Module . (map blockToStatement) . blocks


blockToStatement :: Block -> Js.Statement
blockToStatement block =
  case block of
    Func name impl ->
      let
        toCond (a, s) =
          ( arityComparison a
          , [ Js.Return $ Js.App (toLambda s) $
              map (\x -> Js.Var $ "arguments[" ++ show x ++ "]") [0..a-1]
            ]
          )

        conds = map toCond (Map.toList impl)
      in
        Js.Func name [] [Js.If conds]

    Other s ->
      s


-- UTILS


arityComparison :: Int -> Js.Expr
arityComparison arity =
  Js.Comp Js.Eq
    (Js.Var $ "arguments.length")
    (Js.Lit $ Js.Number $ fromIntegral arity)


toLambda :: Js.Statement -> Js.Expr
toLambda (Js.Func _ params statements) = Js.Lambda params statements


compareBlocks :: Block -> Block -> Bool
compareBlocks (Func name _) (Func name' _) = name == name'
compareBlocks _ _                        = False


putOther :: Js.Statement -> Merge ()
putOther statement =
  modify $ \s -> s { blocks = blocks s ++ [Other statement] }


putFunc :: Js.Statement -> Merge ()
putFunc func@(Js.Func name params _) = do
  blocks' <- gets blocks

  let arity  = length params
  let blocksO = case findFunc name blocks' of
                Just f@(Func _ impl) ->
                  deleteBy compareBlocks f blocks' ++
                  [Func name $ Map.insert arity func impl]

                Nothing ->
                  blocks' ++ [Func name $ Map.singleton arity func]

  modify $ \s -> s { blocks = blocksO }


findFunc :: Name -> [Block] -> Maybe Block
findFunc name = find predicate
  where
    predicate block =
      case block of
        Func name' _ ->
          name == name'

        Other {} ->
          False


-- MERGERS


module' :: Js.Module -> Merge ()
module' (Js.Module statements) = void $ mapM_ statement statements


statement :: Js.Statement -> Merge ()
statement statement =
  case statement of
    Js.Func {} ->
      putFunc statement

    _ ->
      putOther statement
