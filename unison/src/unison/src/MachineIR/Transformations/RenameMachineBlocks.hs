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
module MachineIR.Transformations.RenameMachineBlocks
    (renameMachineBlocks) where

import qualified Data.Map as M

import Common.Util
import MachineIR

renameMachineBlocks mf @ MachineFunction {mfBlocks = code} _target =
  let ids = zip (map mbId code) [0..]
      idf = applyMap $ M.fromList ids
      mf' = mapToMachineBlockId always idf mf
  in mf'
