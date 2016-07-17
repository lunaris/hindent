{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Will Jones' style.

module HIndent.Styles.WillJones where

import HIndent.Pretty
import HIndent.Types

import Control.Monad.State.Class
import Data.Int
import Language.Haskell.Exts.Annotated.Syntax

--------------------------------------------------------------------------------
-- Style configuration

data State =
  State

willJones :: Style
willJones =
  Style {styleName = "will-jones"
        ,styleAuthor = "Will Jones"
        ,styleDescription = "Will Jones' personal style."
        ,styleInitialState = State
        ,styleExtenders =
          [Extender prettyDecl]
        ,styleDefConfig =
          defaultConfig {configMaxColumns = 80
                        ,configIndentSpaces = 2
                        }
        ,styleCommentPreprocessor = return}

--------------------------------------------------------------------------------
-- Extenders

-- | Pretty print a declaration
prettyDecl :: Decl NodeInfo -> Printer s ()
prettyDecl (TypeDecl _ syn ty) =
  prettyTySynDecl syn ty
prettyDecl (TypeSig _ names ty) =
  prettyTySig names ty
prettyDecl (PatSynSig loc name mtyVars mctx1 mctx2 ty) =
  prettyPatSynSig loc name mtyVars mctx1 mctx2 ty
prettyDecl e = prettyNoExt e

-- | Pretty print a type synonym like
--
-- type Foo a
--   =  forall m n.
--      (Monad m,
--       Monad n)
--   => m (n a)
--   -> n (m a)
--
prettyTySynDecl :: (MonadState (PrintState s) m, Pretty ast)
                => ast NodeInfo
                -> Type NodeInfo
                -> m ()
prettyTySynDecl syn ty =
  do write "type "
     pretty syn
     newline
     depend (write "  =  ")
            (declTy ty)

-- | Pretty print a type signature like
--
-- fooBar
--  :: (Applicative f,
--      Monad m)
--  => (a -> f b)
--  -> m a
--  -> f (m b)
--
prettyTySig :: (MonadState (PrintState s) m, Pretty ast)
            => [ast NodeInfo]
            -> Type NodeInfo
            -> m ()
prettyTySig names ty =
  do depend (do inter (write ", ")
                      (map pretty names)
                newline
                write "  :: ")
            (declTy ty)

-- | Pretty print a pattern synonym type signature like
--
-- Foo
--  :: (Applicative f,
--      Applicative g)
--  => (Monad m,
--      Monad n)
--  => f (g a)
--  -> T (m (n a))
--
prettyPatSynSig :: (MonadState (PrintState s) m, Pretty ast)
                => NodeInfo
                -> ast NodeInfo
                -> Maybe [TyVarBind NodeInfo]
                -> Maybe (Context NodeInfo)
                -> Maybe (Context NodeInfo)
                -> Type NodeInfo
                -> m ()
prettyPatSynSig loc name mtyVars mctx1 mctx2 ty =
  do write "pattern "
     pretty name
     newline
     depend (write "  :: ")
            (declTy (TyForall loc mtyVars mctx1
                    (TyForall loc Nothing mctx2 ty)))

declTy ty =
  case ty of
    TyForall _ mtyVars mctx ty' ->
      do case mtyVars of
           Nothing -> return ()
           Just tyVars ->
             do write "forall "
                spaced (map pretty tyVars)
                write "."
                newline
         case mctx of
           Nothing -> declTy ty'
           Just ctx ->
             do prettyCtx ctx
                newline
                indented (-3)
                         (depend (write "=> ")
                                 (declTy ty'))
    _ ->
      prettyTy ty

prettyWithComma x =
  do pretty x
     comma

prettyCtx ctx
  = case ctx of
      CxSingle _ asst ->
        pretty asst
      CxTuple _ assts ->
        parens (prefixedLined " "
                              (mapButLast prettyWithComma pretty assts))
      CxEmpty _ ->
        return ()

prettyTy ty =
  case collapseFunApps ty of
    [] -> pretty ty
    tys ->
      prefixedLined "-> "
                    (map pretty tys)

collapseFunApps (TyFun _ arg result) = arg : collapseFunApps result
collapseFunApps e = [e]

mapButLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapButLast _ g [x] = [g x]
mapButLast f g (x : xs) = f x : mapButLast f g xs
mapButLast _ _ [] = []

-- | Does printing the given thing overflow column limit? (e.g. 80)
fitsOnOneLine :: MonadState (PrintState s) m => m a -> m (Bool,PrintState s)
fitsOnOneLine p =
  do line <- gets psLine
     (_,st) <- sandbox p
     columnLimit <- getColumnLimit
     return (psLine st == line && psColumn st < columnLimit,st)

-- | Is the given expression "small"? I.e. does it fit under
-- 'smallColumnLimit' columns.
isSmallFitting :: MonadState (PrintState t) m
               => m a -> m (Bool,PrintState t)
isSmallFitting p =
  do (_,st) <- sandbox p
     return (psColumn st < smallColumnLimit,st)

smallColumnLimit :: Int64
smallColumnLimit = 50
