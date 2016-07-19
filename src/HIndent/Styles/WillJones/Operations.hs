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

groupParagraphs
  :: Annotated ast
  => [ast NodeInfo] -> [[ast NodeInfo]]
groupParagraphs [] = []
groupParagraphs (initAst:rest) =
  reverse $ map reverse $ go [[initAst]] initEnd rest
  where (_,initEnd) = startEnd initAst
        go
          :: Annotated ast
          => [[ast NodeInfo]] -> Int -> [ast NodeInfo] -> [[ast NodeInfo]]
        go accs _ [] = accs
        go (acc:accs) groupEnd (ast:asts) =
          let (astStart,astEnd) = startEnd ast
          in if astStart - groupEnd > 1
                then go ([ast] : acc : accs) astEnd asts
                else go ((ast : acc) : accs) astEnd asts
        go _ _ _ = error "groupParagraphs/go: impossible"
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
