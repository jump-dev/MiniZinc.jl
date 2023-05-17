# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# N-queens example

# alldifferent formulation
function test_nqueens()
    # based on MiniZinc example nqueens.mzn
    n = 4 # number of queens
    model = MOI.Utilities.Model{Int}()
    # queen in column i is in row q[i]
    q = MOI.add_variables(model, n)
    MOI.add_constraint.(model, q, MOI.Interval(1, n))
    MOI.add_constraint(model, MOI.VectorOfVariables(q), MOI.AllDifferent(n))
    for op in (:+, :-)
        fs = Any[MOI.ScalarNonlinearFunction(op, Any[q[i], i]) for i in 1:n]
        f = MOI.ScalarNonlinearFunction(:alldifferent, Any[fs])
        MOI.add_constraint(model, f, MOI.EqualTo(1))
    end
    # solve
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    q_sol = MOI.get(solver, MOI.VariablePrimal(), [index_map[v] for v in q])
    @test allunique(q_sol)
    @test allunique(q_sol .+ (1:n))
    @test allunique(q_sol .- (1:n))
    rm("test.mzn")
    return
end
