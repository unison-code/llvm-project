/*
 *  Main authors:
 *    Mikael Almgren <mialmg@kth.se>
 *    Roberto Castaneda Lozano <rcas@acm.org>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2015-2016, Mikael Almgren
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *  3. Neither the name of the copyright holder nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef __RELAXED_MODEL__
#define __RELAXED_MODEL__

#include "model.hpp"

using namespace Gecode;
using namespace std;

class RelaxedModel : public Model {

public:

  // Parameters

  // af: aggressiveness factor in allocation branching
  double af;

  void set_aggressiveness(double a1) {af = a1;};

  // Variable accessors

  vector<temporary> & T() const;

  vector<temporary> & O() const;

  vector<temporary> & P() const;

  BoolVar x(operand p) const;

  BoolVar u(operand p, temporary t) const;

  IntVar lat(operand p, temporary t) const;

  BoolVar p(operation o, operation j);

  IntVar s(operand p) const;

  // These are needed since inherited from Model

  IntVar f(block, unsigned int) const { GECODE_NEVER; return v_i[0]; };

  IntVarArgs cost() const { GECODE_NEVER; return v_i; };

  // Auxiliary

  BoolVar relaxed_adhoc_constraint_var(UnisonConstraintExpr & e);

  // Gecode space methods

  RelaxedModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  RelaxedModel(RelaxedModel& cg);

  RelaxedModel* copy(void);

  // Constraint posters
  void post_relaxed_decision_variable_domain_definitions(void);
  void post_relaxed_secondary_variable_definitions(void);
  void post_core_constraints(void);
  void post_instruction_constraints(void);

/*********************************************************************************
 * Core constraints
 ********************************************************************************/
  void post_relaxed_nogood_constraints(void);

/*********************************************************************************
 * Branching
 ********************************************************************************/
  void post_active_operation_branching(vector<operation> O);
  void post_operand_temporary_branching(vector<operand> P);
  void post_instruction_operation_branching(vector<operation> O);
};

#endif
