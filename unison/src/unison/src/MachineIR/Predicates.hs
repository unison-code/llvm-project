{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org

Predicate functions for the machine IR program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module MachineIR.Predicates
       (
         -- * MachineFunctionProperty predicates
         isMachineFunctionPropertyRegisters,
         isMachineFunctionPropertyConstants,
         isMachineFunctionPropertyFixedFrame,
         isMachineFunctionPropertyFrame,
         isMachineFunctionPropertyJumpTable,
         isMachineFunctionPropertyRemovedFreqs,
         isMachineFunctionPropertyVersion,
         -- * MachineBlockProperty predicates
         isMachineBlockPropertyFreq,
         isMachineBlockPropertySuccs,
         isMachineBlockPropertySplit,
         -- * MachineInstruction predicates
         isMachineVirtual,
         isMachineTarget,
         isMachineBranch,
         isMachineCallOrTailCall,
         isMachineLast,
         isMachineExtractSubReg,
         isMachineInsertSubReg,
         isMachineRegSequence,
         isMachineSubregToReg,
         isMachinePhi,
         isMachineCopy,
         isMachineCFIInstruction,
         isMachineEHLabel,
         -- * MachineInstructionProperty predicates
         isMachineInstructionPropertyMem,
         isMachineInstructionPropertyCustom,
         isMachineInstructionPropertyCustomOf,
         isMachineInstructionPropertyJTIBlocks,
         isMachineInstructionPropertyDefs,
         isMachineInstructionPropertyBranchTaken,
         -- * MachineOperand predicates
         isMachineReg,
         isMachineImm,
         isMachineBlockRef,
         isMachineFrameIndex,
         isMachineFrameObject,
         isMachineMemPartition,
         isMachineProperty,
         isMachineNullReg,
         isMachineDebugLocation,
         isMachineTemp,
         isMachineSubRegIndex,
         isMachineConstantPoolIndex,
         isMachineRegImplicit,
         isMachineRegImplicitDef,
         -- * MachineRegState predicates
         isMachineRegUndef,
         -- * MachineFunction predicates
         isMachinePreUnison
       )
       where

import MachineIR.Base

import Unison.Base

isMachineFunctionPropertyRegisters MachineFunctionPropertyRegisters {} = True
isMachineFunctionPropertyRegisters _ = False

isMachineFunctionPropertyConstants MachineFunctionPropertyConstants {} = True
isMachineFunctionPropertyConstants _ = False

isMachineFunctionPropertyFixedFrame MachineFunctionPropertyFixedFrame {} = True
isMachineFunctionPropertyFixedFrame _ = False

isMachineFunctionPropertyFrame MachineFunctionPropertyFrame {} = True
isMachineFunctionPropertyFrame _ = False

isMachineFunctionPropertyJumpTable MachineFunctionPropertyJumpTable {} = True
isMachineFunctionPropertyJumpTable _ = False

isMachineFunctionPropertyRemovedFreqs MachineFunctionPropertyRemovedFreqs {} = True
isMachineFunctionPropertyRemovedFreqs _ = False

isMachineFunctionPropertyVersion MachineFunctionPropertyVersion {} = True
isMachineFunctionPropertyVersion _ = False

isMachineBlockPropertyFreq MachineBlockPropertyFreq {} = True
isMachineBlockPropertyFreq _ = False

isMachineBlockPropertySuccs MachineBlockPropertySuccs {} = True
isMachineBlockPropertySuccs _ = False

isMachineBlockPropertySplit MachineBlockPropertySplit {} = True
isMachineBlockPropertySplit _ = False

isMachineVirtual MachineSingle {msOpcode = MachineVirtualOpc {}} = True
isMachineVirtual _ = False

isMachineTarget MachineSingle {msOpcode = MachineTargetOpc {}} = True
isMachineTarget _ = False

isMachineBranch itf MachineBundle {mbInstrs = mis} =
  any (isMachineBranch itf) mis
isMachineBranch _ mi
  | isMachineVirtual mi = False
isMachineBranch itf MachineSingle {msOpcode = MachineTargetOpc i} =
  itf i == BranchInstructionType

isMachineCallOrTailCall itf MachineBundle {mbInstrs = mis} =
  any (isMachineCallOrTailCall itf) mis
isMachineCallOrTailCall _ mi
  | isMachineVirtual mi = False
isMachineCallOrTailCall itf MachineSingle {msOpcode = MachineTargetOpc i} =
  itf i == CallInstructionType || itf i == TailCallInstructionType

isMachineLast MachineSingle {msOpcode = MachineVirtualOpc opc}
  | opc `elem` [EXIT, RETURN] = True
isMachineLast _ = False

isMachineExtractSubReg
  MachineSingle {msOpcode = MachineVirtualOpc EXTRACT_SUBREG} = True
isMachineExtractSubReg _ = False

isMachineInsertSubReg
  MachineSingle {msOpcode = MachineVirtualOpc INSERT_SUBREG} = True
isMachineInsertSubReg _ = False

isMachineRegSequence
  MachineSingle {msOpcode = MachineVirtualOpc REG_SEQUENCE} = True
isMachineRegSequence _ = False

isMachineSubregToReg
  MachineSingle {msOpcode = MachineVirtualOpc SUBREG_TO_REG} = True
isMachineSubregToReg _ = False

isMachinePhi MachineSingle {msOpcode = MachineVirtualOpc PHI} = True
isMachinePhi _ = False

isMachineCopy MachineSingle {msOpcode = MachineVirtualOpc COPY} = True
isMachineCopy _ = False

isMachineCFIInstruction
  MachineSingle {msOpcode = MachineVirtualOpc CFI_INSTRUCTION} = True
isMachineCFIInstruction _ = False

isMachineEHLabel MachineSingle {msOpcode = MachineVirtualOpc EH_LABEL} = True
isMachineEHLabel _ = False

isMachineInstructionPropertyMem MachineInstructionPropertyMem {} = True
isMachineInstructionPropertyMem _ = False

isMachineInstructionPropertyCustom MachineInstructionPropertyCustom {} = True
isMachineInstructionPropertyCustom _ = False

isMachineInstructionPropertyCustomOf text
  MachineInstructionPropertyCustom {msPropertyCustom = text'}
  | text == text' = True
isMachineInstructionPropertyCustomOf _ _ = False

isMachineInstructionPropertyJTIBlocks MachineInstructionPropertyJTIBlocks {} = True
isMachineInstructionPropertyJTIBlocks _ = False

isMachineInstructionPropertyDefs MachineInstructionPropertyDefs {} = True
isMachineInstructionPropertyDefs _ = False

isMachineInstructionPropertyBranchTaken MachineInstructionPropertyBranchTaken {} = True
isMachineInstructionPropertyBranchTaken _ = False

isMachineReg MachineReg {} = True
isMachineReg _ = False

isMachineImm MachineImm {} = True
isMachineImm _ = False

isMachineBlockRef MachineBlockRef {} = True
isMachineBlockRef _ = False

isMachineFrameIndex MachineFrameIndex {} = True
isMachineFrameIndex _ = False

isMachineFrameObject MachineFrameObject {} = True
isMachineFrameObject _ = False

isMachineMemPartition MachineMemPartition {} = True
isMachineMemPartition _ = False

isMachineProperty MachineProperty {} = True
isMachineProperty _ = False

isMachineNullReg MachineNullReg {} = True
isMachineNullReg _ = False

isMachineDebugLocation MachineDebugLocation {} = True
isMachineDebugLocation _ = False

isMachineConstantPoolIndex MachineConstantPoolIndex {} = True
isMachineConstantPoolIndex _ = False

isMachineRegImplicit MachineReg {mrFlags = fs}
  | MachineRegImplicit `elem` fs = True
isMachineRegImplicit _ = False

isMachineRegImplicitDef MachineReg {mrFlags = fs}
  | MachineRegImplicitDefine `elem` fs = True
isMachineRegImplicitDef _ = False

isMachineTemp MachineTemp {} = True
isMachineTemp _ = False

isMachineSubRegIndex MachineSubRegIndex {} = True
isMachineSubRegIndex _ = False

isMachineRegUndef MachineRegUndef {} = True
isMachineRegUndef _ = False

isMachinePreUnison mf =
  let mis = concatMap mbInstructions (mfBlocks mf)
  in any isMachinePreUnisonInstruction mis

isMachinePreUnisonInstruction MachineBundle {} = False
isMachinePreUnisonInstruction MachineSingle {msOperands = mos} =
  any isMachinePreUnisonOperand mos

isMachinePreUnisonOperand MachineTemp {} = True
isMachinePreUnisonOperand MachineSubTemp {} = True
isMachinePreUnisonOperand MachineSubRegIndex {} = True
isMachinePreUnisonOperand MachineFrameIndex {} = True
isMachinePreUnisonOperand MachineFrameObject {} = True
isMachinePreUnisonOperand MachineFrameSize {} = True
isMachinePreUnisonOperand _ = False
