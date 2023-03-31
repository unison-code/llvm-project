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
module MachineIR.Transformations.DropExplicitFallthroughs
    (dropExplicitFallthroughs) where

import qualified Data.Map as M

import Unison
import Unison.Target.API
import MachineIR

dropExplicitFallthroughs mf target =
  let itf = instructionType target
      oif = operandInfo target
      bif = branchInfo target
      (Natural nf) = nop target
      n = oTargetInstr $ fromSingleton $ oIs nf
      fs  = (itf, oif, bif, n)
      mbs = mfBlocks mf
      ftb = buildFallThroughMap mbs
      mf' = mf {mfBlocks = map (dropExplicitFallthrough fs ftb) mbs}
  in mf'

buildFallThroughMap mbs =
  M.fromList $ map fallThrough (zip (mkMachineBlock (-1) [] [] : mbs) mbs)

fallThrough (mb1, mb2) = (mbId mb1, mbId mb2)

dropExplicitFallthrough fs ftb
  mb @ MachineBlock {mbId = id, mbInstructions = mis}
  | null mis || not (M.member id ftb) = mb
  | otherwise =
    let mis' = concatMap (filterUnconditionalJumps fs (ftb M.! id)) mis
    in mb {mbInstructions = mis'}

filterUnconditionalJumps fs @ (_, _, _, n) l
  mb @ MachineBundle {mbInstrs = mis} =
    case filter (not . isUnconditionalJumpTo fs l) mis of
      []   -> []
      [MachineSingle {msOpcode = MachineTargetOpc i}]
        | i == n -> []
      [mi] -> [mi]
      mis' -> [mb {mbInstrs = mis'}]
filterUnconditionalJumps fs l mi
    | isUnconditionalJumpTo fs l mi = []
    | otherwise = [mi]

isUnconditionalJumpTo fs l MachineBundle {mbInstrs = mis} =
    any (isUnconditionalJumpTo fs l) mis
isUnconditionalJumpTo (itf, oif, bif, _) l mi
  | isMachineBranch itf mi =
    let i = fromMachineInstruction itf oif (-1, mi)
    in case bif i of
      Just (BranchInfo Unconditional (Just l')) -> l == l'
      _ -> False
  | otherwise = False
