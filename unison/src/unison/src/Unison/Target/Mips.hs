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
module Unison.Target.Mips (target) where

import qualified Data.Set as S
import Data.Maybe
import Data.List.Split
import Control.Arrow
import Data.List

import MachineIR hiding (parse)

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Target.Query
import Unison.Analysis.TemporaryType
import Unison.Analysis.TransitiveOperations
import Unison.Transformations.FoldReservedRegisters

import Unison.Target.Mips.Registers
import Unison.Target.Mips.Transforms
import Unison.Target.Mips.Usages
import Unison.Target.Mips.Common
import Unison.Target.Mips.MipsRegisterDecl
import Unison.Target.Mips.MipsResourceDecl
import Unison.Target.Mips.SpecsGen.MipsRegisterClassDecl
import Unison.Target.Mips.SpecsGen.MipsInstructionDecl
import qualified Unison.Target.Mips.SpecsGen as SpecsGen

target =
    API.TargetDescription {
      API.tRegisterArray    = const registerArray,
      API.tRegisterAtoms    = const registerAtoms,
      API.tRegClasses       = const regClasses,
      API.tRegisters        = const registers,
      API.tInfRegClassUsage = const infRegClassUsage,
      API.tInfRegClassBound = const infRegClassBound,
      API.tSubRegIndexType  = const subRegIndexType,
      API.tCallerSaved      = const callerSaved,
      API.tCalleeSaved      = const calleeSaved,
      API.tReserved         = const reserved,
      API.tInstructionType  = const SpecsGen.instructionType,
      API.tBranchInfo       = const branchInfo,
      API.tPreProcess       = const preProcess,
      API.tPostProcess      = postProcess,
      API.tTransforms       = const transforms,
      API.tCopies           = const copies,
      API.tRematInstrs      = const rematInstrs,
      API.tFromCopy         = const fromCopy,
      API.tOperandInfo      = operandInfo,
      API.tAlignedPairs     = const SpecsGen.alignedPairs,
      API.tPackedPairs      = const (const (const [])),
      API.tRelatedPairs     = const (const []),
      API.tResources        = const resources,
      API.tUsages           = usages,
      API.tNop              = const nop,
      API.tReadWriteInfo    = const SpecsGen.readWriteInfo,
      API.tImplementFrame   = const implementFrame,
      API.tAddPrologue      = const addPrologue,
      API.tAddEpilogue      = const addEpilogue,
      API.tStackDirection   = const stackDirection,
      API.tReadWriteLatency = readWriteLatency,
      API.tAlternativeTemps = const alternativeTemps,
      API.tExpandCopy       = const expandCopy,
      API.tConstraints      = const constraints,
      API.tSpillOverhead    = const spillOverhead
    }

instance Read MipsInstruction where
  readsPrec _ strOp = [(SpecsGen.readOp strOp, "")]

-- | Gives the type of natural instruction according to the operation

-- | Gives the target of a jump instruction and the type of jump

branchInfo (Branch {oBranchIs = is, oBranchUs = [_, BlockRef l]})
  | targetInst is `elem`
    expandBranches [BGEZ, BLTZ, BGTZ, BLEZ, BEQZC, BEQZALC, BNEZC, BNEZALC, BC1F, BC1T] =
    BranchInfo Conditional (Just l)
branchInfo (Branch {oBranchIs = is, oBranchUs = [_, _, BlockRef l]})
  | targetInst is `elem` expandBranches [BEQ, BNE] = BranchInfo Conditional (Just l)
branchInfo (Branch {oBranchIs = is, oBranchUs = [BlockRef l]})
  | targetInst is `elem` expandBranches [B, J] = BranchInfo Unconditional (Just l)
branchInfo (Branch {oBranchIs = is})
  | targetInst is `elem` expandBranches [JR, RetRA, PseudoIndirectBranch, PseudoReturn] =
    BranchInfo Unconditional Nothing
branchInfo o = error ("unmatched pattern: branchInfo " ++ show (mkSingleOperation (-1) (Natural o)))

expandBranches = concatMap addDelaySlotNOPInstr

-- | Gives a set of def copies and a list of sets of use copies to extend
-- the given temporary

-- Do not extend temporaries that are congruent to temporaries pre-allocated
-- to reserved registers
copies _fInfo _phiTemp _t [r] _d us | r `elem` reserved =
    ([], replicate (length us) [])

-- Do not extend temporaries that are defined by virtual defines
copies _fInfo False _t _rs d [_]
  | isDefine d ||
    (isNatural d && targetInst (oInstructions d) `elem` [CLOBBER_RA]) =
      ([], [[]])

-- Do not extend temporaries that are only used by virtual kills
copies _fInfo False _t _rs _d [u] | isKill u = ([], [[]])

-- Add only one store for entry calle-saved temporaries
-- Add only one load for exit calle-saved temporaries
-- Do not add copies for intermediate calle-saved temporaries
copies (f, cst, _, _, _, _) False t [r] _d [_u]
  | S.member t cst =
    (
      if isEntryTemp (fCode f) t
      then [mkNullInstruction, TargetInstruction (pushInstruction r)]
      else [],
      [if isExitTemp (fCode f) t
       then [mkNullInstruction, TargetInstruction (popInstruction r)]
       else []]
    )

-- Extend temporaries defined in acc64 with mflo and mfhi only
copies _ False _ [] d us
    | isNatural d && targetInst (oInstructions d) `elem`
      [PseudoMULT, PseudoMADD, PseudoMTLOHI, MULT, MULTu, DIV, MADD] =
      ([], replicate (length us) [])
copies _ False t [] d us
    | isLow d  = ([mkNullInstruction, TargetInstruction MFLO], map (accCopy t) us)
    | isHigh d = ([mkNullInstruction, TargetInstruction MFHI], map (accCopy t) us)

-- Do not extend temporaries with the address used by function calls
copies _ False _ _ d [u]
    | isFunctionPointerLoad d && isJALRCall u = ([], [[]])

-- Extend temporaries combined to be used by a macc with mtlo and mthi
copies _ False t [] _ [u] | isCombine u = (accCopy t u, [[]])
copies _ False _ [] d us | isCombine d =
  ([], replicate (length us) [])

-- Do not extend rematerializable instructions used only once, locally
-- FIXME: review whether this is always safe
copies (Function {fCode = code}, _, _, _, _, _) False t _ d [u]
  | isNatural d && (isNatural u || isFun u) &&
    (isRematerializable (targetInst (oInstructions d))) &&
    not (mayCrossMemDep SpecsGen.readWriteInfo d u code) &&
    compatibleClassesForTemp t [d, u] = ([], [[]])

copies (f, _, cg, ra, bcfg, sg) _ t _ d us =
  let is = d:us
      w  = widthOfTemp ra cg f t is
      -- This below is just to determine which temporaries require only 32-bits
      -- FP rather than GP copies
      drcs = transitiveRegClasses (operandInfo []) bcfg sg Reaching f t
      dors = transitivePreAssignments bcfg sg Reaching f t
      urcs = transitiveRegClasses (operandInfo []) bcfg sg Reachable f t
      uors = transitivePreAssignments bcfg sg Reachable f t
      rcType rcs ors
        | null rcs && null ors = AnyRegClass
        | any isFGR32Class rcs || any isFGR32 ors = FGR32RegClass
        | any isAFGR64Class rcs || any isAFGR64 ors = AFGR64RegClass
        | otherwise = GPR32RegClass
      (drc, urc) = (rcType drcs dors, rcType urcs uors)
  in (defCopies drc w,
      map (const (useCopies urc w)) us)

data RegClassType =
  AnyRegClass |
  FGR32RegClass |
  AFGR64RegClass |
  GPR32RegClass
  deriving Show

defCopies GPR32RegClass  1 = [mkNullInstruction] ++ map TargetInstruction [MOVE, STORE]
defCopies FGR32RegClass  1 = [mkNullInstruction] ++ map TargetInstruction [MOVE_F, STORE_F]
defCopies AnyRegClass    1 = [mkNullInstruction] ++ map TargetInstruction [MOVE, MOVE_F, STORE, STORE_F]
defCopies _              2 = [mkNullInstruction] ++ map TargetInstruction [MOVE_D, STORE_D]
defCopies rc w = error ("unmatched: defCopies " ++ show rc ++ " " ++ show w)

useCopies GPR32RegClass  1 = [mkNullInstruction] ++ map TargetInstruction [MOVE, LOAD]
useCopies FGR32RegClass  1 = [mkNullInstruction] ++ map TargetInstruction [MOVE_F, LOAD_F]
useCopies AnyRegClass    1 = [mkNullInstruction] ++ map TargetInstruction [MOVE, MOVE_F, LOAD, LOAD_F]
useCopies _              2 = [mkNullInstruction] ++ map TargetInstruction [MOVE_D, LOAD_D]
useCopies rc w = error ("unmatched: useCopies " ++ show rc ++ " " ++ show w)

classOfTemp = classOf (target, [])
widthOfTemp = widthOf (target, [])

compatibleClassesForTemp t os =
  let regs = [S.fromList $ registers $ fromJust (classOfTemp t o) | o <- os]
  in not $ S.null $ foldl S.intersection (head regs) regs

isFGR32Class rc = rc `elem` map RegisterClass [FGR32, FGR32Opnd]
isFGR32 r = mipsReg r `elem` registers (RegisterClass FGR32Opnd)

isAFGR64Class rc = rc `elem` map RegisterClass [AFGR64, AFGR64Opnd]
isAFGR64 r = mipsReg r `elem` registers (RegisterClass AFGR64Opnd)

mipsReg = rTargetReg . regId

accCopy t i
  | isCombine i && isCombineLowOf t i  = [mkNullInstruction, TargetInstruction MTLO]
  | isCombine i && isCombineHighOf t i = [mkNullInstruction, TargetInstruction MTHI]
  | otherwise = []

pushInstruction r
  | r `elem` registers (RegisterClass GPR32Opnd) = STORE
  | r `elem` registers (RegisterClass AFGR64Opnd) = STORE_D

popInstruction r
  | r `elem` registers (RegisterClass GPR32Opnd) = LOAD
  | r `elem` registers (RegisterClass AFGR64Opnd) = LOAD_D

isFunctionPointerLoad SingleOperation {
  oOpr = (Natural (Linear {
                      oIs = [TargetInstruction LW],
                      oUs = [Temporary {},
                             Bound MachineGlobalAddress {}]}))} = True
isFunctionPointerLoad _ = False

isJALRCall SingleOperation {
  oOpr = (Natural (Call {oCallIs = [TargetInstruction JALRPseudo]}))} = True
isJALRCall _ = False

-- | Transforms copy instructions into natural instructions
fromCopy o @ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [MOVE, MFLO, MFHI, MTLO, MTHI] = toLinear o
  | i `elem` [MOVE_F, MOVE_D] = toLinear  (o { oCopyIs = [TargetInstruction $ fromCopyInstr i] })
  | i `elem` [STORE, STORE_F, STORE_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs  = [s, mkOprMipsSP, mkBoundMachineFrameObject i d],
            oDs  = []}
  | i `elem` [LOAD, LOAD_F, LOAD_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs  = [mkOprMipsSP, mkBoundMachineFrameObject i s],
            oDs  = [d]}
fromCopy (Natural o) = o
fromCopy o = error ("unmatched pattern: fromCopy " ++ show o)

mkOprMipsSP = Register $ mkTargetRegister SP

mkBoundMachineFrameObject i (Register r) =
    let size = stackSize i
    in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size False)

stackSize op
  | op `elem` [STORE, STORE_F, LOAD, LOAD_F] = 4
  | op `elem` [STORE_D, LOAD_D] = 8

fromCopyInstr = fromJust . SpecsGen.parent

rematInstrs i
  | i `elem` [CLOBBER_RA] = Nothing
  | isRematerializable i =
      Just (sourceInstr i, dematInstr i, rematInstr i)
  | otherwise = Nothing

-- | Declares target architecture resources

resources =
  [
    -- Upper bound of what can be issued in parallel
    Resource BundleWidth 2,
    Resource Issue 1,
    Resource LongDuration 1,
    Resource ALU 1,
    Resource IMULDIV 1
  ]

-- | No-operation instruction

nop = Linear [TargetInstruction NOP] [] []

-- | Implementation of frame setup and destroy operations

implementFrame = const []

mkOpt oid i us ds =
  let o  = mkLinear oid [mkNullInstruction, TargetInstruction i] us ds
      o' = addActivators (map TargetInstruction spUsers) o
  in o'

addActivators = mapToActivators . (++)

-- | Adds function prologue

addPrologue (_, oid, _) (e:l:code)
  | isMandNaturalWith ((==) LoadGPDisp) l = [e, l, mkAddiu_negsp oid] ++ code
addPrologue (_, oid, _) (e:code) = [e, mkAddiu_negsp oid] ++ code

mkAddiu_negsp oid = mkOpt oid ADDiu_negsp [Bound mkMachineFrameSize] []

-- | Adds function epilogue

addEpilogue (_, oid, _) code =
    let [f, e] = split (keepDelimsL $ whenElt isBranch) code
        addSp = mkOpt oid ADDiu_sp [Bound mkMachineFrameSize] []
    in f ++ [addSp] ++ e

spUsers = (filter isSPUser SpecsGen.allInstructions) \\ [ADDiu_sp, ADDiu_negsp]
isSPUser = readsObject (OtherSideEffect SP)
readsObject rwo i = rwo `elem` (fst $ SpecsGen.readWriteInfo i)

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Target dependent pre-processing functions

preProcess = [addFrameIndex, explicateRA]

addFrameIndex = mapToTargetMachineInstruction addFrameIndexInstr

addFrameIndexInstr mi @ MachineSingle {msOpcode = opcode,
                                       msOperands = operands}
  | any isMachineFrameIndex operands &&
    any isTemporaryInfo (fst $ operandInfo [] $ mopcTarget opcode) =
      mi {msOpcode = liftToTOpc (\i -> read (show i ++ "_fi")) opcode}
  | otherwise = mi

explicateRA = mapToTargetMachineInstruction explicateRAInInstr

explicateRAInInstr mi @ MachineSingle {msOpcode = MachineTargetOpc RetRA} =
  mi {msOpcode   = mkMachineTargetOpc PseudoReturn,
      msOperands = [mkMachineReg RA]}
explicateRAInInstr mi = mi

liftToTOpc f = mkMachineTargetOpc . f . mopcTarget

-- | Target dependent post-processing functions

postProcess to = [expandPseudosEarly to, if keepNops to then id else cleanNops,
                  expandPseudos, unbundleSingletons, removeFrameIndex,
                  normalizeDelaySlots to]

expandPseudosEarly to = mapToMachineBlock (expandBlockPseudos
                                           (expandPseudoEarly to))

expandPseudoEarly to mi @ MachineSingle {msOpcode = MachineTargetOpc LoadGPDisp}
  | not (unitLatency to) =
  let v0  = mkMachineReg V0
      gpd = mkMachineExternal "_gp_disp"
      mi1 = mi {msOpcode = mkMachineTargetOpc LUi, msOperands = [v0, gpd]}
      mi2 = mi {msOpcode = mkMachineTargetOpc ADDiu, msOperands = [v0, v0, gpd]}
  in [[mi1],[mi2]]

expandPseudoEarly _ mi @ MachineSingle {msOpcode = MachineTargetOpc PseudoCVT_S_W,
                                   msOperands = [fi, ri]} =
  let mi1 = mi {msOpcode = mkMachineTargetOpc MTC1, msOperands = [fi, ri]}
      mi2 = mi {msOpcode = mkMachineTargetOpc CVT_S_W, msOperands = [fi, fi]}
  in [[mi1],[mi2]]
expandPseudoEarly _ mi = [[mi]]

expandPseudos = mapToMachineBlock (expandBlockPseudos expandPseudo)

expandPseudo mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | isDelaySlotNOPInstr i =
  let mi1 = mi {msOpcode = mkMachineTargetOpc (delaySlotInstr i)}
      mi2 = mkMachineSingle (mkMachineTargetOpc NOP) [] []
  in [[mi1, mi2]]

expandPseudo mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                 msOperands = mos} =
  [[expandSimple mi (i, mos)]]
expandPseudo mi = [[mi]]

expandSimple mi (RetRA, _) =
  mi {msOpcode = MachineTargetOpc PseudoReturn,
      msOperands = [mkMachineReg RA]}

expandSimple mi (i, [MachineImm mfs])
  | i `elem` [ADDiu_sp, ADDiu_negsp] =
      let mfs' = if i == ADDiu_negsp then (-mfs) else mfs
      in  mi {msOpcode   = MachineTargetOpc ADDiu,
              msOperands = [mkMachineReg SP, mkMachineReg SP,
                            mkMachineImm mfs']}

expandSimple mi (i, [v, off])
  | i `elem` [SW_sp, SWC1_sp] =
    mi {msOpcode   = MachineTargetOpc (fromJust $ SpecsGen.parent i),
        msOperands = [v, mkMachineReg SP, off]}

expandSimple mi (MOVE, [d, s]) =
  mi {msOpcode   = MachineTargetOpc OR,
      msOperands = [d, s, mkMachineReg ZERO]}

expandSimple mi _ = mi

cleanNops = filterMachineInstructions (not . isSingleNop)

isSingleNop MachineSingle {msOpcode = MachineTargetOpc NOP} = True
isSingleNop _ = False

-- Unbundle singleton bundles created during pseudo expansion.
unbundleSingletons = mapToMachineBlock unbundleSingletonsInBlock

unbundleSingletonsInBlock mb @ MachineBlock {mbInstructions = mis} =
  mb {mbInstructions = map unbundleSingleton mis}

unbundleSingleton MachineBundle {mbInstrs = [mi]} = mi
unbundleSingleton mi = mi

removeFrameIndex = mapToMachineInstruction removeFrameIndexInstr

removeFrameIndexInstr mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                          msOperands = mops}
  | "_fi" `isSuffixOf`  (show i) =
    let mops' = case mops of
                  [r @ MachineReg {}, off @ MachineImm {},
                   MachineImm {miValue = 0}] ->
                    [r, mkMachineReg SP, off]
                    -- FIXME: post-process other patterns similary.
                  _ -> mops
      in mi {msOpcode = mkMachineTargetOpc $ removeFi i, msOperands = mops'}
  | otherwise = mi

removeFi i = read $ dropSuffix "_fi" (show i)

normalizeDelaySlots to = mapToMachineBlock (normalizeDelaySlotInBlock to)

normalizeDelaySlotInBlock to mb @ MachineBlock {mbInstructions = mis} =
  let mis1 = concatMap (normalizeDelaySlot to) mis
      mis2 = map removeBundleHead mis1
  in mb {mbInstructions = mis2}

normalizeDelaySlot _ mb @ MachineBundle {
  mbInstrs = [mi, mbi @ MachineSingle {msOpcode = MachineTargetOpc i}]}
  | isDelaySlotInstr i = [mb {mbInstrs = [mbi, mi]}]
normalizeDelaySlot to MachineBundle {
  mbInstrs = [mi, mbi @ MachineSingle {msOpcode = MachineTargetOpc i}]}
  | isDelaySlotInstr i && noDelaySlots to = [mi, mbi]
normalizeDelaySlot to MachineBundle {
  mbInstrs = [mbi @ MachineSingle {msOpcode = MachineTargetOpc i}, mi]}
  | isDelaySlotInstr i && noDelaySlots to = [mi, mbi]
normalizeDelaySlot _ mi = [mi]

-- This assumes all remaining bundles are branches with delay slots.
removeBundleHead mb @ MachineBundle {} = mb {mbHead = False}
removeBundleHead mi = mi

-- | Gives a list of function transformers
transforms ImportPreLift = [peephole rs2ts,
                            peephole normalizeCallPrologue,
                            peephole normalizeCallEpilogue,
                            peephole extractReturnRegs,
                            (\f -> foldReservedRegisters f (target, [])),
                            mapToOperation hideStackPointer,
                            coupleAcc64Operations,
                            mapToOperation addAlternativeInstructions]

transforms ImportPostLift = [peephole clobberRAInCall]

transforms AugmentPreRW = [peephole insertGPDisp]

transforms AugmentPostRW = [mapToOperation markBarriers,
                            peephole enforceMandatoryFrame]

transforms ExportPreLow = [cleanClobbers]

transforms _ = []

-- | Latency of read-write dependencies

readWriteLatency _ _ (_, Read) (_, Write) = 0
readWriteLatency _ _ ((_, VirtualType (DelimiterType InType)), _) (_, _) = 1
readWriteLatency _ _ ((_, VirtualType FunType), _) (_, _) = 1
readWriteLatency _ _ ((_, VirtualType _), _) (_, _) = 0
readWriteLatency to _ ((TargetInstruction p, _), _) (_, _) =
    maybeMax 0 $ map occupation (usages to p)


-- | Alternative temporaries of each operand

-- All temps that hold the same value
alternativeTemps _ _ _ ts = map fst ts

-- | Copy expansion

expandCopy _ _ o = [o]

-- | Custom processor constraints

constraints f =
  -- force RA clobbering operations to be scheduled one cycle before their
  -- corresponding call operation (that is, two cycles before the corresponding
  -- (fun) operation):
  clobberRASchedulingConstraints f ++
  -- force delay slot operations without bundled NOPs to be scheduled in
  -- parallel with other non-virtual operations in their basic blocks:
  filledDelaySlotConstraints f

clobberRASchedulingConstraints f =
  let fcode = flatCode f
  in [clobberRASchedulingConstraint fcode o | o <- fcode, isClobberRA o]

clobberRASchedulingConstraint fcode clo =
  let [d]  = extractTemps (oSingleDef clo)
      [fo] = potentialUsers d fcode
  in DistanceExpr (oId fo) (oId clo) (-2)

filledDelaySlotConstraints f = mapMaybe filledDelaySlotConstraint (fCode f)

filledDelaySlotConstraint Block {bCode = code} =
  case find isDelaySlotOpr code of
   Just o ->
     let oid = oId o
         i   = fromJust $ find (\(TargetInstruction i) -> isDelaySlotNOPInstr i)
               (oInstructions o)
         os  = [oId o' | o' <- code, o' /= o, not (isVirtual o')]
     in Just (OrExpr
              ([ImplementsExpr oid i] ++
               [AndExpr [ActiveExpr oid',
                         DistanceExpr oid oid' 0,
                         DistanceExpr oid' oid 0] | oid' <- os]))
   Nothing -> Nothing

isDelaySlotOpr o =
  isNatural o && isDelaySlotInstr (targetInst (oInstructions o))

operandInfo to i =
  adjustDefLatency to i $ correctUses i $ SpecsGen.operandInfo i

correctUses i info
    | i `elem` [LW, LH, LBu, LB, LHu] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0 False, BoundInfo],
         [TemporaryInfo (RegisterClass GPR32Opnd) 1 False])
    | i `elem` [LWL, LWR] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0 False, BoundInfo,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False],
         [TemporaryInfo (RegisterClass GPR32Opnd) 1 False])
    | i `elem` [SW, SB, SH, SWL, SWR] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0 False,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False, BoundInfo],
         [])
    | i `elem` [LDC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0 False, BoundInfo],
         [TemporaryInfo (RegisterClass AFGR64Opnd) 1 False])
    | i `elem` [SWC1] =
        ([TemporaryInfo (RegisterClass FGR32Opnd) 0 False,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False,
          BoundInfo],
         [])
    | i `elem` [SDC1] =
        ([TemporaryInfo (RegisterClass AFGR64Opnd) 0 False,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False,
          BoundInfo],
         [])
    | i `elem` [LWC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0 False, BoundInfo],
         [TemporaryInfo (RegisterClass FGR32Opnd) 1 False])
    | i `elem` [LWXC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0 False,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False],
         [TemporaryInfo (RegisterClass FGR32Opnd) 1 False])
    | i `elem` [LDXC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0 False,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False],
         [TemporaryInfo (RegisterClass AFGR64Opnd) 1 False])
    | i `elem` [SWXC1] =
        ([TemporaryInfo (RegisterClass FGR32Opnd) 0 False,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False,
          TemporaryInfo (RegisterClass GPR32Opnd) 0 False],
         [])
    | otherwise = info

adjustDefLatency to i = second (map (applyToLatency (const (latency to i))))

latency' i = latency [] i

spillOverhead (i, _:sp:_, _)
  | i `elem` [SW, SB, SDC1, SWC1] && sp == mkOprMipsSP =
    Just (True, latency' i)
spillOverhead (i, sp:_, _)
  | i `elem` [LW, LBu, LDC1, LWC1] && sp == mkOprMipsSP =
    Just (False, latency' i)
spillOverhead (i, _, _)
  | i `elem` [SW_fi, SB_fi, SWC1_fi] =
    Just (True, latency' i)
spillOverhead (i, _, _)
  | i `elem` [LW_fi, LBu_fi, LDC1_fi, LWC1_fi] =
    Just (False, latency' i)
spillOverhead _ = Nothing
