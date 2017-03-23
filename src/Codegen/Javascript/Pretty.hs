{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.Javascript.Pretty where

import           Control.Monad.State
import           Data.List
import           Text.PrettyPrint

import qualified Codegen.Javascript.Syntax as JS


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


prettyPrint :: JS.Module -> String
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


pModule :: JS.Module -> Printer ()
pModule (JS.Module sts) = do
    mapM_ pStatement sts


pStatement :: JS.Statement -> Printer ()
pStatement st = case st of
    JS.Func name params sts -> do
        appendN $ (func name params) <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        appendN rbrace
        newline

    JS.If cases -> do
        pIf cases

    JS.Export name ->
        appendN $ export name

    JS.Return expr -> do
        appendN $ return' <> space
        pExpr expr
        append semi

    JS.Expr expr -> do
        newline
        pExpr expr
        append semi


pIf :: [(JS.Expr, [JS.Statement])] -> Printer ()
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


pExpr :: JS.Expr -> Printer ()
pExpr expr = case expr of
    JS.Lit lit ->
        pLit lit

    JS.Var var ->
        append $ text var

    JS.App fn args -> do
        append lparen
        pExpr fn
        append $ rparen <> lparen
        mapIntersperse_ (append commaSpc) pExpr args
        append rparen

    JS.Lambda params sts -> do
        append $ func "" params <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        appendN rbrace

    JS.Comp JS.Eq e1 e2 -> do
        pExpr e1
        append compareEq
        pExpr e2


pLit :: JS.Lit -> Printer ()
pLit lit = case lit of
    JS.String str ->
        append $ quotes $ text str

    JS.Bool bool ->
        append $ text $ if bool then "true" else "false"

    JS.Number num ->
        append $ text $ show num
