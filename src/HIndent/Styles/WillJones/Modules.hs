{-# LANGUAGE OverloadedStrings #-}

module HIndent.Styles.WillJones.Modules where

import HIndent.Pretty
import HIndent.Styles.WillJones.Operations
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
         whenJust prettyModuleHead mmdHead
         pretty (Module l Nothing [] importDecls decls)
    _ ->
      prettyNoExt md

-- | Pretty print a module head
prettyModuleHead :: ModuleHead NodeInfo -> Printer s ()
prettyModuleHead (ModuleHead _ name mwarning mexports) =
  do write "module "
     pretty name
     whenJust pretty mwarning
     whenJust (\exports -> newline >> prettyExportSpecList exports) mexports

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

-- | Pretty print an export list like
--
-- ( T (..)
-- , f
--
-- , pattern P1
-- , mkX
-- , xToY
--
-- , module M1
-- )
--
prettyExportSpecList :: ExportSpecList NodeInfo -> Printer s ()
prettyExportSpecList (ExportSpecList _ specs) =
  do depend (write "  ( ") $
       inter (newline >> comma) $
        flip map (groupParagraphs specs) $ \specGroup -> do
          prefixedLined ", " (map prettyExportSpec specGroup)
          newline
     write "  ) "

prettyExportSpec :: ExportSpec NodeInfo -> Printer s ()
prettyExportSpec sp =
  do prettyNoExt sp
