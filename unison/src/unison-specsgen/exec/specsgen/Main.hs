{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Main (main) where

import SpecsGen.Driver

main = runSpecsGen id (\_ _ -> return ())
