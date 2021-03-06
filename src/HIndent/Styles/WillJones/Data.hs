{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HIndent.Styles.WillJones.Data where

import HIndent.Pretty
import HIndent.Styles.WillJones.Operations
import HIndent.Styles.WillJones.Types
import HIndent.Types

import Control.Monad.State
import Data.List
import Data.Ord
import Language.Haskell.Exts.Annotated.Syntax

-- | Pretty prints a newtype declaration like
--
-- newtype (Monad m) => Foo m a
--   = ...
prettyNewtypeDecl :: MonadState (PrintState s) m
                  => Maybe (Context NodeInfo)
                  -> DeclHead NodeInfo
                  -> QualConDecl NodeInfo
                  -> Maybe (Deriving NodeInfo)
                  -> m ()
prettyNewtypeDecl mctx declHead con mderiv =
  do write "newtype "
     whenJust pretty mctx
     pretty declHead
     newline
     write "  = "
     prettyQualConDecl con
     whenJust prettyDeriving mderiv

-- | Pretty prints a data declaration like
--
-- data (Monad m) => Foo m a
--   = ...
prettyDataDecl :: MonadState (PrintState s) m
               => Maybe (Context NodeInfo)
               -> DeclHead NodeInfo
               -> [QualConDecl NodeInfo]
               -> Maybe (Deriving NodeInfo)
               -> m ()
prettyDataDecl mctx declHead cons mderiv =
  do write "data "
     whenJust pretty mctx
     pretty declHead
     newline
     write "  = "
     prefixedLined "  | "
                   (map prettyQualConDecl cons)
     whenJust prettyDeriving mderiv

prettyQualConDecl :: MonadState (PrintState s) m
                  => QualConDecl NodeInfo
                  -> m ()
prettyQualConDecl qDecl =
  case qDecl of
    QualConDecl _ mtyVars mctx conDecl ->
      do whenJust prettyForall mtyVars
         whenJust prettyCtx mctx
         prettyConDecl conDecl

prettyConDecl :: MonadState (PrintState s) m
              => ConDecl NodeInfo
              -> m ()
prettyConDecl conDecl =
  case conDecl of
    RecDecl _ name fields ->
      prettyRecDecl name fields
    _ ->
      pretty conDecl

-- | Pretty prints a record constructor like
--
-- FooRecord
--   { firstField
--       :: forall a b m n.
--          (Monad m,
--           Monad n)
--       => m a
--       -> n b
--       -> m (n (a, b))
--
--   , secondField
--       :: Int
--       -> Char
--       -> X
--
--   }
--
prettyRecDecl :: MonadState (PrintState s) m
              => Name NodeInfo
              -> [FieldDecl NodeInfo]
              -> m ()
prettyRecDecl name fields =
  do pretty name
     newline
     indented 4 (do depend (write "  { ")
                           (prefixedLined ", "
                                          (map prettyFieldDecl fields))
                    newline
                    write "  }")

prettyFieldDecl :: MonadState (PrintState s) m
                => FieldDecl NodeInfo
                -> m ()
prettyFieldDecl (FieldDecl _ names ty)
  = case names of
      [name] ->
        do pretty name
           newline
           depend (write "  :: ")
                  (prettyTy ty)
           newline
      _ ->
        fail "Record fields must be given separate types, even if they are the same"

-- | Pretty prints a deriving clause like
--
-- deriving (Eq, JSON.FromJSON, Ord, PG.FromRow, Show,
--           Typeable)
--
prettyDeriving :: MonadState (PrintState s) m
               => Deriving NodeInfo -> m ()
prettyDeriving (Deriving _ rules) =
  do write "deriving ("
     inter (write ", ")
           (map pretty (sortBy (comparing instRuleNameString) rules))
     write ")"

instRuleNameString :: InstRule l -> String
instRuleNameString (IRule _ _ _ ih) = instHeadNameString ih
instRuleNameString (IParen _ rule) = instRuleNameString rule

instHeadNameString :: InstHead l -> String
instHeadNameString (IHCon _ qn) = qNameString qn
instHeadNameString (IHInfix _ _ qn) = qNameString qn
instHeadNameString (IHParen _ ih) = instHeadNameString ih
instHeadNameString (IHApp _ ih _) = instHeadNameString ih
