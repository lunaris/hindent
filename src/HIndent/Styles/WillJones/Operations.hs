{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HIndent.Styles.WillJones.Operations where

import HIndent.Pretty
import HIndent.Types

import Control.Monad.State
import Data.Int
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc

--------------------------------------------------------------------------------
-- Utilities

whenJust :: Applicative f => (a -> f ()) -> Maybe a -> f ()
whenJust
  = maybe (pure ())

qNameString :: QName l -> String
qNameString (Qual _ m n) = moduleNameString m ++ "." ++ nameString n
qNameString (UnQual _ n) = nameString n
qNameString (Special _ s) = specialConString s

specialConString :: SpecialCon l -> String
specialConString (UnitCon _) = "()"
specialConString (ListCon _) = "[]"
specialConString (FunCon _) = "->"
specialConString (TupleCon _ boxed n) =
  case boxed of
    Boxed -> '(' : cs ++ ")"
    Unboxed -> '(' : '#' : cs ++ "#)"
  where cs = replicate (n - 1) ','
specialConString (Cons _) = ":"
specialConString (UnboxedSingleCon _) = "(# #)"

moduleNameString :: ModuleName l -> String
moduleNameString (ModuleName _ modName) = modName

nameString :: Name l -> String
nameString (Ident _ name) = name
nameString (Symbol _ name) = name

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

withParagraphs :: (Annotated ast
                  ,MonadState (PrintState s) m)
               => ([ast NodeInfo] -> m ())
               -> ([ast NodeInfo] -> m ())
               -> [ast NodeInfo]
               -> m ()
withParagraphs _ _ [] = return ()
withParagraphs withFirst withLater (initAst:rest) =
  go (withFirst . (initAst :)) initEnd rest
  where (_,initEnd) = startEnd initAst
        go f _ [] = f []
        go f groupEnd (ast:asts) =
          let (astStart,astEnd) = startEnd ast
          in if astStart - groupEnd > 1
                then do f []
                        go (withLater . (ast :)) astEnd asts
                else do go (f . (ast :)) astEnd asts
        startEnd :: Annotated ast
                 => ast NodeInfo -> (Int,Int)
        startEnd ast =
          case ann ast of
            NodeInfo (SrcSpanInfo s _) _ ->
              (srcSpanStartLine s,srcSpanEndLine s)

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
