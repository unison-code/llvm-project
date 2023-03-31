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
module Unison.Target.Mips.SpecsGen (module X) where
  import Unison.Target.Mips.SpecsGen.ReadWriteInfo as X
  import Unison.Target.Mips.SpecsGen.OperandInfo as X
  import Unison.Target.Mips.SpecsGen.ReadOp as X
  import Unison.Target.Mips.SpecsGen.ShowInstance()
  import Unison.Target.Mips.SpecsGen.AllInstructions as X
  import Unison.Target.Mips.SpecsGen.Itinerary as X
  import Unison.Target.Mips.SpecsGen.InstructionType as X
  import Unison.Target.Mips.SpecsGen.AlignedPairs as X
  import Unison.Target.Mips.SpecsGen.Parent as X
  import Unison.Target.Mips.SpecsGen.Size as X
