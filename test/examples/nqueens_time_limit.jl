# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# N-queens example
# based on MiniZinc example nqueens.mzn
# queen in column i is in row q[i]

# Test solver time out 
function test_nqueens_timeout()
    n = 100 # can't finish in 1s
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
        with_bridge_type = Int,
    )
    MOI.set(model, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    q = MOI.add_variables(model, n)
    MOI.add_constraint.(model, q, MOI.Interval(1, n))
    MOI.add_constraint(model, MOI.VectorOfVariables(q), MOI.AllDifferent(n))
    for op in (+, -)
        f = MOI.Utilities.vectorize([op(q[i], i) for i in eachindex(q)])
        MOI.add_constraint(model, f, MOI.AllDifferent(n))
    end

    # test timeout in 1s
    MOI.set(model, MOI.TimeLimitSec(), 1.0)
    # solve
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.TIME_LIMIT
    @test MOI.get(model, MOI.ResultCount()) == 0
    rm("test.mzn")
    return
end
