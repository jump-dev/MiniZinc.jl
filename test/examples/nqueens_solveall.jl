# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# N-queens example
# based on MiniZinc example nqueens.mzn
# queen in column i is in row q[i]

n = 8

function init_model()
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

function check_result(
    model,
    q,
    actual_count = 92,
    termination_status = MOI.OPTIMAL,
)
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
    return rm("test.mzn")
end

function run_nqueens(option)
    if option == "solve_all1" # solve all
        @info "test solve_all with num_solution not set"
        model, q = init_model()
        MOI.set(model, MOI.RawOptimizerAttribute("all_solutions"), true)
        MOI.optimize!(model)
        check_result(model, q)
    elseif option == "solve_all2" # solve all with limit > 92
        @info "test solve_all with limit > 92"
        model, q = init_model()
        MOI.set(model, MOI.RawOptimizerAttribute("all_solutions"), true)
        MOI.set(model, MOI.RawOptimizerAttribute("num_solutions"), 100)
        MOI.optimize!(model)
        check_result(model, q)
    elseif option == "solve_all3" # solve all with limit = 25 
        @info "test solve_all with limit = 25"
        model, q = init_model()
        MOI.set(model, MOI.RawOptimizerAttribute("all_solutions"), true)
        MOI.set(model, MOI.RawOptimizerAttribute("num_solutions"), 25)
        MOI.optimize!(model)
        check_result(model, q, 25, MOI.SOLUTION_LIMIT)
    elseif option == "solve_one" # solve one
        @info "test solve_one"
        model, q = init_model()
        MOI.set(model, MOI.RawOptimizerAttribute("all_solutions"), false)
        MOI.set(model, MOI.RawOptimizerAttribute("num_solutions"), 100)
        MOI.optimize!(model)
        check_result(model, q, 1)
    elseif option == "throw"
        @info "test throw"
        model, q = init_model()
        @test_throws ErrorException MOI.set(
            model,
            MOI.RawOptimizerAttribute("all_solutions"),
            1,
        )
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
    return
end

test_nqueens1() = run_nqueens("solve_all1")
test_nqueens2() = run_nqueens("solve_all2")
test_nqueens3() = run_nqueens("solve_all3")
test_nqueens4() = run_nqueens("solve_one")
test_nqueens5() = run_nqueens("throw")
