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
     whenJust pretty mderiv

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
     whenJust pretty mderiv

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

--------------------------------------------------------------------------------
-- Utilities

whenJust :: Applicative f => (a -> f ()) -> Maybe a -> f ()
whenJust
  = maybe (pure ())

commaSpaceBefore :: MonadState (PrintState s) m
                 => (a -> m ())
                 -> a
                 -> m ()
commaSpaceBefore f x =
  do write ", "
     f x

commaAfter :: MonadState (PrintState s) m
           => (a -> m ())
           -> a
           -> m ()
commaAfter f x =
  do f x
     comma

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

mapButLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapButLast _ g [x] =
  [g x]
mapButLast f g (x : xs) =
  f x : mapButLast f g xs
mapButLast _ _ [] =
  []
