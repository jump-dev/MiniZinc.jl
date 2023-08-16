# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# N-queens example
# based on MiniZinc example nqueens.mzn
# queen in column i is in row q[i]

# alldifferent formulation
function test_nqueens_alldiff()
    n = 4
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
    # solve
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    q_sol = MOI.get(model, MOI.VariablePrimal(), q)
    @test allunique(q_sol)
    @test allunique(q_sol .+ (1:n))
    @test allunique(q_sol .- (1:n))
    rm("test.mzn")
    return
end

function test_nqueens_exprtree()
    n = 4
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
    )
    MOI.set(model, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    q = MOI.add_variables(model, n)
    MOI.add_constraint.(model, q, MOI.Interval(1, n))
    for i in 1:n, j in (1+i):n
        snf1 = MOI.ScalarNonlinearFunction(:(!=), Any[q[i], q[j]])
        MOI.add_constraint(model, snf1, MOI.EqualTo(1))
        snf2 = MOI.ScalarNonlinearFunction(:-, Any[q[i], q[j]])
        snf3 = MOI.ScalarNonlinearFunction(:abs, Any[snf2])
        snf4 = MOI.ScalarNonlinearFunction(:(!=), Any[snf3, j-i])
        MOI.add_constraint(model, snf4, MOI.EqualTo(1))
    end
    # solve
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    q_sol = MOI.get(model, MOI.VariablePrimal(), q)
    @test allunique(q_sol)
    @test allunique(q_sol .+ (1:n))
    @test allunique(q_sol .- (1:n))
    rm("test.mzn")
    return
end
