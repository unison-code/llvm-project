/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, RISE SICS AB
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


#include "options.hpp"

ModelOptions::ModelOptions(void)
  : InstanceOptions("Unison"),

    _output_file("-o", "output file", ""),
    _output_every_iteration("--output-every-iteration", "produce an output file every iteration (only works if an output file is set)", false),
    _verbose("--verbose", "verbose mode", false),
#ifdef GRAPHICS
    _gist_global("--gist-global", "run Gist for global problems", false),
    _gist_block("--gist-block", "block on which to run Gist", -1),
    _gist_iteration("--gist-iteration", "iteration on which to run Gist", -1),
    _gist_presolving("--gist-presolving", "run Gist during presolving", false),
    _gist_solution("--gist-solution", "run Gist for the solution", false),
#endif

    _limit_unit("--limit-unit", "unit of limits (time, fails)", "time"),
    _global_budget("--global-budget", "global time budget per operation", 12.0),
    _global_setup_limit("--global-setup-limit", "limit for root global propagation", 1000.0),
    _local_limit("--local-limit", "local limit", 10000.0),
    _global_shaving_limit("--global-shaving-limit", "global shaving limit", 10000.0),
    _post_global_shaving_limit("--post-global-shaving-limit", "global shaving limit after global solver", 1000.0),
    _local_shaving_limit("--local-shaving-limit", "local shaving limit", 2000.0),

    _decomposition("--decomposition", "run decomposition", true),
    _monolithic("--monolithic", "run monolithic solver", true),
    _initial_aggressiveness("--initial-aggressiveness", "initial aggressiveness", 0.0),
    _step_aggressiveness("--step-aggressiveness", "aggressiveness step", 1.0 / 9 - 0.0000001),
    _final_aggressiveness("--final-aggressiveness", "final aggressiveness", 1.0),
    _custom_portfolio("--custom-portfolio", "run custom portfolio", false),
    _local_portfolio("--local-portfolio", "search portfolio for the local problem", "atmfc"),
    _disable_presolving("--disable-presolving", "disable presolving techniques", false),
    _disable_local_shaving("--disable-local-shaving", "disable local shaving techniques", false),
    _disable_global_shaving("--disable-global-shaving", "disable global shaving techniques", false),
    _consistency_threshold("--consistency-threshold", "operation threshold to switch to bounds consistency", 150),
    _shaving_threshold("--shaving-threshold", "pperation threshold to disable shaving", 150),
    _solve_global_only("--solve-global-only", "solve only the global problem", false),
    _first_solution("--first-solution", "return the first solution found", false),
    _solving_threshold("--solving-threshold", "operation threshold to give up on solving", std::numeric_limits<int>::max()),

    _disable_hints("--disable-hints", "disable hints in global solver", false),
    _disable_precedence_variables("--disable-precedence-variables", "disable explicit precedence variables and constraints", false),
    _overconstrain("--overconstrain", "allow to overconstrain model", false),

    _disable_improving("--disable-improving", "disable improved constraints", false),
    _disable_maximum_temporary_usage_constraints("--disable-maximum-temporary-usage-constraints", "disable maximum temporary usage constraints", false),
    _disable_copy_dominance_constraints("--disable-copy-dominance-constraints", "disable copy dominance constraints", false),
    _disable_space_capacity_constraints("--disable-space-capacity-constraints", "disable space capacity constraints", false),
    _disable_operand_symmetry_breaking_constraints("--disable-operand-symmetry-breaking-constraints", "disable operand symmetry breaking constraints", false),
    _disable_presolver_constraints("--disable-presolver-constraints", "disable presolver constraints", false),
    _disable_minimum_number_of_optional_operations_constraints("--disable-minimum-number-of-optional-operations-constraints", "disable minimum number of optional operations constraints", false),
    _disable_allowed_activation_constraints("--disable-allowed-activation-constraints", "disable allowed activation constraints", false),
    _disable_copy_activation_and_dataflow_constraints("--disable-copy-activation-and-dataflow-constraints", "disable copy activation and dataflow constraints", false),
    _disable_nogood_constraints("--disable-nogood-constraints", "disable nogood constraints", false),
    _disable_basic_nogood_constraints("--disable-basic-nogood-constraints", "disable basic nogood constraints (corresponding to the 'nogoods' parameter)", false),
    _disable_additional_nogood_constraints("--disable-additional-nogood-constraints", "disable additional nogood constraints (corresponding to the 'nogoods2' parameter)", false),
    _disable_conditional_precedence_constraints("--disable-conditional-precedence-constraints", "disable conditional precedence constraints", false),
    _disable_basic_conditional_precedence_constraints("--disable-basic-conditional-precedence-constraints", "disable basic conditional precedence constraints (corresponding to the 'precedences' parameter)", false),
    _disable_additional_conditional_precedence_constraints("--disable-additional-conditional-precedence-constraints", "disable additional conditional precedence constraints (corresponding to the 'precedences2' parameter)", false),
    _disable_partially_ordered_live_range_constraints("--disable-partially-ordered-live-range-constraints", "disable partially ordered live range constraints", false),
    _disable_basic_partially_ordered_live_range_constraints("--disable-basic-partially-ordered-live-range-constraints", "disable basic partially ordered live range constraints (corresponding to the 'before' parameter)", false),
    _disable_additional_partially_ordered_live_range_constraints("--disable-additional-partially-ordered-live-range-constraints", "disable additional partially ordered live range constraints (corresponding to the 'before2' parameter)", false),
    _disable_across_call_disjoint_temporary_constraints("--disable-across-call-disjoint-temporary-constraints", "disable across call disjoint temporary constraints", false),
    _disable_across_call_disjoint_temporary_set_constraints("--disable-across-call-disjoint-temporary-set-constraints", "disable across call disjoint temporary set constraints", false),
    _disable_temporary_symmetry_breaking_constraints("--disable-temporary-symmetry-breaking-constraints", "disable temporary symmetry breaking constraints", false),
    _disable_infinite_register_dominance_constraints("--disable-infinite-register-dominance-constraints", "disable infinite register dominance constraints", false)

{
  add(_output_file);
  add(_output_every_iteration);
  add(_verbose);
#ifdef GRAPHICS
  add(_gist_global);
  add(_gist_block);
  add(_gist_iteration);
  add(_gist_presolving);
  add(_gist_solution);
#endif

  add(_limit_unit);
  add(_global_budget);
  add(_global_setup_limit);
  add(_local_limit);
  add(_global_shaving_limit);
  add(_post_global_shaving_limit);
  add(_local_shaving_limit);

  add(_decomposition);
  add(_monolithic);
  add(_initial_aggressiveness);
  add(_step_aggressiveness);
  add(_final_aggressiveness);
  add(_custom_portfolio);
  add(_local_portfolio);
  add(_disable_presolving);
  add(_disable_local_shaving);
  add(_disable_global_shaving);
  add(_consistency_threshold);
  add(_shaving_threshold);
  add(_solve_global_only);
  add(_first_solution);
  add(_solving_threshold);

  add(_disable_hints);
  add(_disable_precedence_variables);
  add(_overconstrain);

  add(_disable_improving);
  add(_disable_maximum_temporary_usage_constraints);
  add(_disable_copy_dominance_constraints);
  add(_disable_space_capacity_constraints);
  add(_disable_operand_symmetry_breaking_constraints);
  add(_disable_presolver_constraints);
  add(_disable_minimum_number_of_optional_operations_constraints);
  add(_disable_allowed_activation_constraints);
  add(_disable_copy_activation_and_dataflow_constraints);
  add(_disable_nogood_constraints);
  add(_disable_basic_nogood_constraints);
  add(_disable_additional_nogood_constraints);
  add(_disable_conditional_precedence_constraints);
  add(_disable_basic_conditional_precedence_constraints);
  add(_disable_additional_conditional_precedence_constraints);
  add(_disable_partially_ordered_live_range_constraints);
  add(_disable_basic_partially_ordered_live_range_constraints);
  add(_disable_additional_partially_ordered_live_range_constraints);
  add(_disable_across_call_disjoint_temporary_constraints);
  add(_disable_across_call_disjoint_temporary_set_constraints);
  add(_disable_temporary_symmetry_breaking_constraints);
  add(_disable_infinite_register_dominance_constraints);
}
