# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# N-queens example - solve_all
# based on MiniZinc example nqueens.mzn
# queen in column i is in row q[i]

function _init_model()
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

function _check_result(
    model,
    q,
    actual_count = 92,
    termination_status = MOI.OPTIMAL,
)
    n = 8
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

function test_solve_all1() # solve all with limit > 92
    @info "test solve_all with limit > 92"
    model, q = _init_model()
    MOI.set(model, MOI.RawOptimizerAttribute("num_solutions"), 100)
    MOI.optimize!(model)
    return _check_result(model, q)
end

function test_solve_all2() # solve all with limit = 25 
    @info "test solve_all with limit = 25"
    model, q = _init_model()
    MOI.set(model, MOI.RawOptimizerAttribute("num_solutions"), 25)
    MOI.optimize!(model)
    return _check_result(model, q, 25, MOI.SOLUTION_LIMIT)
end

function test_solve_one1() # solve one with limit not set
    @info "test solve_one with limit not set"
    model, q = _init_model()
    MOI.optimize!(model)
    return _check_result(model, q, 1)
end

function test_solve_one2() # solve one with limit = 1
    @info "test solve_one with limit = 1"
    model, q = _init_model()
    MOI.set(model, MOI.RawOptimizerAttribute("num_solutions"), 1)
    MOI.optimize!(model)
    return _check_result(model, q, 1)
end

function test_throw() # test throw
    @info "test throw"
    model, _ = _init_model()
    @test_throws ErrorException MOI.set(
        model,
        MOI.RawOptimizerAttribute("num_solutions"),
        -1,
    )
    @test_throws ErrorException MOI.set(
        model,
        MOI.RawOptimizerAttribute("num_solutions"),
        0,
    )
    @test_throws ErrorException MOI.set(
        model,
        MOI.RawOptimizerAttribute("num_solutions"),
        1.1,
    )
    @test_throws ErrorException MOI.set(
        model,
        MOI.RawOptimizerAttribute("num_solutions"),
        "two",
    )
end
