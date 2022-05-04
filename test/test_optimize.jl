# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function test_moi_basic_fzn()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(1))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = MiniZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) in [1, 2, 3]
    return
end

function test_moi_infeasible_fzn()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(5))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = MiniZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    _, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OTHER_ERROR
    @test MOI.get(solver, MOI.ResultCount()) == 0
    return
end

function test_moi_one_solution_fzn()
    model = MOI.Utilities.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1, 10))
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    solver = MiniZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) == 10
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    solver = MiniZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) == 1
    return
end

function test_moi_int_lin()
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(model, 3)
    MOI.add_constraint.(model, x, MOI.Integer())
    for i in 1:3
        MOI.add_constraint(model, 2 * x[i], MOI.GreaterThan(0))
        MOI.add_constraint(model, 1 * x[i], MOI.LessThan(1))
    end
    MOI.add_constraint(model, sum(1 * x[i] for i in 1:3), MOI.EqualTo(2))
    solver = MiniZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    v = [MOI.get(solver, MOI.VariablePrimal(), index_map[xi]) for xi in x]
    @test all(v .>= 0)
    @test all(v .<= 1)
    @test sum(v) == 2
    return
end

function test_moi_all_different()
    model = MOI.Utilities.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1, 3))
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, y, MOI.Interval(1, 3))
    z, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, z, MOI.Interval(1, 3))
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x, y, z]),
        MOI.AllDifferent(3),
    )
    solver = MiniZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    v = [
        MOI.get(solver, MOI.VariablePrimal(), index_map[xi]) for xi in [x, y, z]
    ]
    @test v[1] != v[2] && v[2] != v[3] && v[1] != v[3]
    return
end
