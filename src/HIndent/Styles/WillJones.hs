{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Will Jones' style.

module HIndent.Styles.WillJones where

import HIndent.Pretty
import HIndent.Styles.WillJones.Data
import HIndent.Styles.WillJones.Modules
import HIndent.Styles.WillJones.Types
import HIndent.Types

import Control.Monad.State
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
          [Extender prettyModule
          ,Extender prettyModuleHead
          ,Extender prettyDecl]
        ,styleDefConfig =
          defaultConfig {configMaxColumns = 80
                        ,configIndentSpaces = 2
                        }
        ,styleCommentPreprocessor = return}

--------------------------------------------------------------------------------
-- Extenders

-- | Pretty print a declaration
prettyDecl :: Decl NodeInfo -> Printer s ()
prettyDecl (TypeDecl _ declHead ty) =
  prettyTySynDecl declHead ty
prettyDecl (TypeFamDecl _ declHead mkind) =
  prettyTyFamDecl declHead mkind
prettyDecl (DataDecl _ dataOrNewtype mctx declHead cons mderiv) =
  case dataOrNewtype of
    DataType _ ->
      prettyDataDecl mctx declHead cons mderiv
    NewType _ ->
      case cons of
        [con] ->
          prettyNewtypeDecl mctx declHead con mderiv
        _ ->
          fail "Data types declared using 'newtype' must have exactly one constructor"
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
prettyTySynDecl :: (MonadState (PrintState s) m)
                => DeclHead NodeInfo
                -> Type NodeInfo
                -> m ()
prettyTySynDecl declHead ty =
  do write "type "
     pretty declHead
     newline
     depend (write "  =  ")
            (prettyTy ty)

-- | Pretty print a type family declaration like
--
-- type family Foo (a :: *) (m :: * -> *)
--   :: *
--   -> *
prettyTyFamDecl :: (MonadState (PrintState s) m)
                => DeclHead NodeInfo
                -> Maybe (Kind NodeInfo)
                -> m ()
prettyTyFamDecl declHead mkind =
  do write "type family "
     pretty declHead
     case mkind of
       Nothing -> return ()
       Just kind ->
         do newline
            depend (write "  :: ")
                   (prettyKind kind)

-- | Pretty print a type signature like
--
-- fooBar
--  :: (Applicative f,
--      Monad m)
--  => (a -> f b)
--  -> m a
--  -> f (m b)
--
prettyTySig :: (MonadState (PrintState s) m)
            => [Name NodeInfo]
            -> Type NodeInfo
            -> m ()
prettyTySig names ty =
  do depend (do inter (write ", ")
                      (map pretty names)
                newline
                write "  :: ")
            (prettyTy ty)

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
prettyPatSynSig :: (MonadState (PrintState s) m)
                => NodeInfo
                -> Name NodeInfo
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
            (prettyTy (TyForall loc mtyVars mctx1
                    (TyForall loc Nothing mctx2 ty)))
