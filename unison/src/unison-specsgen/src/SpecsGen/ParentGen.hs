{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module SpecsGen.ParentGen (emitParent) where

import Language.Haskell.Syntax

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitParent targetName is =
    let us2ids = infoToIds iParent is
        rhss   = map (mkOpcRhs idToHsCon toParentRhs) us2ids
    in [hsModule
        (moduleName targetName "Parent")
        (Just [hsExportVar "parent"])
        [instructionDeclImport targetName]
        [simpleOpcFunBind "parent" rhss]]

toParentRhs Nothing = toHsCon "Nothing"
toParentRhs (Just p) = (HsApp (toHsCon "Just") (toHsCon $ toOpType p))
