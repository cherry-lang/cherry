{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.Javascript.Pretty where

import           Control.Monad.State
import           Data.List
import           Text.PrettyPrint

import qualified Codegen.Javascript.Syntax as Js


data PrinterState
    = PrinterState
    { doc :: Doc
    , ind :: Int
    }
    deriving (Show)


newtype Printer a = Printer { runPrinter :: State PrinterState a }
    deriving (Functor, Applicative, Monad, MonadState PrinterState)


indent :: Printer ()
indent = modify $ \s -> s { ind = 4 + ind s }


dedent :: Printer ()
dedent = modify $ \s -> s { ind = (ind s) - 4 }


appendN :: Doc -> Printer ()
appendN doc' = modify $ \s -> s { doc = doc s $+$ nest (ind s) doc' }


append :: Doc -> Printer ()
append doc' = modify $ \s -> s { doc = doc s <> nest (ind s) doc' }


newline :: Printer ()
newline = appendN $ text ""


emptyPrinterState :: PrinterState
emptyPrinterState = PrinterState empty 0


execPrinter :: Printer a -> Doc
execPrinter m = doc $ execState (runPrinter m) emptyPrinterState


prettyPrint :: Js.Module -> String
prettyPrint mod' = render $ execPrinter $ pModule mod'


mapIntersperse_ :: Monad m => m b -> (a -> m b) -> [a] -> m ()
mapIntersperse_ _ _ []       = return ()
mapIntersperse_ _ fn (x:[])  = fn x >> return ()
mapIntersperse_ sp fn (x:xs) = do
    _ <- fn x
    _ <- sp
    mapIntersperse_ sp fn xs


----------------------------------------------------------------
-- Parts
----------------------------------------------------------------


func :: String -> [String] -> Doc
func name params =
    empty <>
    text "function" <+>
    text name <>
    parens (hsep $ punctuate comma $ map text params)


export :: String -> Doc
export name =
    empty <>
    text "export" <+>
    text name <>
    semi


return' :: Doc
return' = text "return"


compareEq :: Doc
compareEq = space <> text "===" <> space


commaSpc :: Doc
commaSpc = comma <> space


----------------------------------------------------------------
-- Printers
----------------------------------------------------------------


pModule :: Js.Module -> Printer ()
pModule (Js.Module sts) = do
    mapM_ pStatement sts


pStatement :: Js.Statement -> Printer ()
pStatement st = case st of
    Js.Func name params sts -> do
        appendN $ (func name params) <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        appendN rbrace
        newline

    Js.If cases -> do
        pIf cases

    Js.Export name ->
        appendN $ export name

    Js.Return expr -> do
        appendN $ return' <> space
        pExpr expr
        append semi

    Js.Expr expr -> do
        newline
        pExpr expr
        append semi

    Js.Skip ->
      return ()


pIf :: [(Js.Expr, [Js.Statement])] -> Printer ()
pIf ifs = do
    let h = head ifs
    let t = tail ifs

    if_ "if" h
    mapM_ (if_ "else if") t
  where
    if_ if' (cond, sts) = do
        appendN $ text if' <+> lparen
        pExpr cond
        append $ rparen <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        appendN rbrace


pExpr :: Js.Expr -> Printer ()
pExpr expr = case expr of
    Js.Lit lit ->
        pLit lit

    Js.Var var ->
        append $ text var

    Js.App fn args -> do
        append lparen
        pExpr fn
        append $ rparen <> lparen
        mapIntersperse_ (append commaSpc) pExpr args
        append rparen

    Js.Lambda params sts -> do
        append $ func "" params <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        appendN rbrace

    Js.Comp Js.Eq e1 e2 -> do
        pExpr e1
        append compareEq
        pExpr e2


pLit :: Js.Lit -> Printer ()
pLit lit = case lit of
    Js.String str ->
        append $ quotes $ text str

    Js.Bool bool ->
        append $ text $ if bool then "true" else "false"

    Js.Number num ->
        append $ text $ show num

    Js.Void ->
      return ()
