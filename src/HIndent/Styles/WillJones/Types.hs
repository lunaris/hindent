{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HIndent.Styles.WillJones.Types where

import HIndent.Pretty
import HIndent.Styles.WillJones.Operations
import HIndent.Types

import Control.Monad.State
import Language.Haskell.Exts.Annotated.Syntax

-- | Pretty print a type like
--
--     forall a b f m.
--     (Applicative f,
--      Monad m)
--  => (a -> f b)
--  -> m a
--  -> f (m b)
--
prettyTy :: MonadState (PrintState s) m
         => Type NodeInfo
         -> m ()
prettyTy ty =
  case ty of
    TyForall _ mtyVars mctx ty' ->
      do whenJust prettyForall mtyVars
         case mctx of
           Nothing -> prettyTy ty'
           Just ctx ->
             do prettyCtx ctx
                newline
                indented (-3)
                         (depend (write "=> ")
                                 (prettyTy ty'))
    _ ->
      prettyFunSplitTy ty

prettyFunSplitTy :: MonadState (PrintState s) m
                 => Type NodeInfo
                 -> m ()
prettyFunSplitTy ty =
  case splitTyFunApps ty of
    [] -> pretty ty
    tys ->
      prefixedLined "-> "
                    (map pretty tys)

splitTyFunApps :: Type l -> [Type l]
splitTyFunApps (TyFun _ arg result) =
  arg : splitTyFunApps result
splitTyFunApps ty =
  [ty]

-- | Pretty print a kind like
--
--    *
-- -> (* -> *)
-- -> k1
-- -> k2
--
prettyKind :: MonadState (PrintState s) m
           => Kind NodeInfo
           -> m ()
prettyKind kind =
  case kind of
    _ ->
      prettyFunSplitKind kind

prettyFunSplitKind :: MonadState (PrintState s) m
                   => Kind NodeInfo
                   -> m ()
prettyFunSplitKind kind =
  case splitKindFunApps kind of
    [] -> pretty kind
    kinds ->
      prefixedLined "-> "
                    (map pretty kinds)

splitKindFunApps :: Kind l -> [Kind l]
splitKindFunApps (KindFn _ arg result)
  = arg : splitKindFunApps result
splitKindFunApps kind =
  [kind]

-- | Pretty print a set of forall-bound type variables like
--
-- forall a b m n.
--
prettyForall :: MonadState (PrintState s) m
             => [TyVarBind NodeInfo]
             -> m ()
prettyForall [] =
  return ()
prettyForall tyVars =
  do write "forall "
     spaced (map pretty tyVars)
     write "."
     newline

-- | Pretty print a context like
--
-- Monad m
--
-- or
--
-- (Monad m,
--  Monad n,
--  Applicative f)
prettyCtx :: MonadState (PrintState s) m
          => Context NodeInfo
          -> m ()
prettyCtx ctx =
  case ctx of
    CxSingle _ asst ->
      pretty asst
    CxTuple _ assts ->
      parens (prefixedLined " "
                            (mapButLast (commaAfter pretty) pretty assts))
    CxEmpty _ ->
      return ()
