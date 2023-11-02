# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function _init_nqueens_solve_num_solutions()
    n = 8
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
    return model, q
end

function _test_nqueens_solve_num_solutions(
    model,
    q,
    actual_count = 92,
    termination_status = MOI.OPTIMAL,
)
    n = 8
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === termination_status
    res_count = MOI.get(model, MOI.ResultCount())
    @test res_count == actual_count
    for i in 1:res_count
        q_sol = MOI.get(model, MOI.VariablePrimal(i), q)
        @test allunique(q_sol)
        @test allunique(q_sol .+ (1:n))
        @test allunique(q_sol .- (1:n))
    end
    @test MOI.get(model, MOI.SolveTimeSec()) < 4.0
    rm("test.mzn")
    return
end

function test_nqueens_solve_num_solutions_100()
    model, q = _init_nqueens_solve_num_solutions()
    MOI.set(model, MOI.SolutionLimit(), 100)
    _test_nqueens_solve_num_solutions(model, q)
    return
end

function test_nqueens_solve_num_solutions_25()
    model, q = _init_nqueens_solve_num_solutions()
    MOI.set(model, MOI.SolutionLimit(), 25)
    _test_nqueens_solve_num_solutions(model, q, 25, MOI.SOLUTION_LIMIT)
    return
end

function test_nqueens_solve_num_solutions_not_set()
    model, q = _init_nqueens_solve_num_solutions()
    _test_nqueens_solve_num_solutions(model, q, 1)
    return
end

function test_nqueens_solve_num_solutions_1()
    model, q = _init_nqueens_solve_num_solutions()
    MOI.set(model, MOI.SolutionLimit(), 1)
    _test_nqueens_solve_num_solutions(model, q, 1)
    return
end

function test_nqueens_num_solutions_throw()
    model, _ = _init_nqueens_solve_num_solutions()
    for value in (-1, 0, 1.1, "two")
        @test_throws(
            MOI.SetAttributeNotAllowed,
            MOI.set(model, MOI.SolutionLimit(), value)
        )
    end
    return
end
