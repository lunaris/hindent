{-# LANGUAGE OverloadedStrings #-}

module HIndent.Styles.WillJones.Modules where

import HIndent.Pretty
import HIndent.Types

import Control.Monad
import Data.List
import Language.Haskell.Exts.Annotated.Syntax

-- | Pretty print a module
prettyModule :: Module NodeInfo -> Printer s ()
prettyModule md =
  case md of
    Module l mmdHead pragmas@(_ : _) importDecls decls ->
      do prettyModulePragmas pragmas
         pretty (Module l mmdHead [] importDecls decls)
    _ ->
      prettyNoExt md

-- | Pretty print a list of module pragmas
prettyModulePragmas :: [ModulePragma NodeInfo] -> Printer s ()
prettyModulePragmas ps =
  do prettyLanguagePragmas [n | LanguagePragma _ ns <- langPs, n <- ns]
     case nonLangPs of
      [] -> return ()
      _ -> inter newline (map pretty nonLangPs)
  where
    (langPs, nonLangPs) = partition isLanguagePragma ps

-- | Pretty print a set of language pragmas like
--
-- {-# LANGUAGE ConstraintKinds            #-}
-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MagicHash                  #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE PatternSynonyms            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE UndecidableInstances       #-}
--
prettyLanguagePragmas :: [Name NodeInfo] -> Printer s ()
prettyLanguagePragmas [] = return ()
prettyLanguagePragmas ns =
  do forM_ sortedNStrings $ \(nString, len) -> do
       let paddingSize = maxLen - len
       write "{-# LANGUAGE "
       string nString
       string (replicate paddingSize ' ')
       write " #-}"
       newline
     newline
  where
    nStrings = map (\n -> let s = nameString n in (s, length s)) ns
    sortedNStrings = nub (sort nStrings)
    maxLen = foldr (max . snd) 0 nStrings

isLanguagePragma :: ModulePragma a -> Bool
isLanguagePragma (LanguagePragma _ _) = True
isLanguagePragma _ = False

nameString :: Name NodeInfo -> String
nameString (Ident _ name) = name
nameString (Symbol _ name) = name
