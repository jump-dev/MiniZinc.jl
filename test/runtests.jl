# Copyright (c) 2022 FlatZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestFlatZinc

using Test
import MathOptInterface
import FlatZinc
import Chuffed_jll

const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _test_chuffed_asset(file, args...)
    filename = joinpath(@__DIR__, "assets", file)
    ret = FlatZinc.run(Chuffed_jll.fznchuffed, filename, args...)
    return replace(ret, "\r\n" => "\n")
end

function test_chuffed_basic()
    @test _test_chuffed_asset("basic.fzn") == "x = 3;\n\n----------\n"
    return
end

function test_chuffed_one_solution()
    @test _test_chuffed_asset("one_solution.fzn") ==
          "x = 10;\n\n----------\n==========\n"
    return
end

function test_chuffed_asset_several_solutions()
    @test _test_chuffed_asset("several_solutions.fzn", ["-a"]) ==
          "xs = array1d(1..2, [2, 3]);\n" *
          "\n" *
          "----------\n" *
          "xs = array1d(1..2, [1, 3]);\n" *
          "\n" *
          "----------\n" *
          "xs = array1d(1..2, [1, 2]);\n" *
          "\n" *
          "----------\n" *
          "==========\n"
    return
end

function test_chuffed_asset_puzzle()
    @test _test_chuffed_asset("puzzle.fzn") ==
          "x = array2d(1..4, 1..4, [5, 1, 8, 8, 9, 3, 8, 6, 9, 7, 7, 8, 1, 7, 8, 9]);" *
          "\n" *
          "\n" *
          "----------\n"
    return
end

function test_chuffed_asset_einstein()
    @test _test_chuffed_asset("einstein.fzn") ==
          "a = array1d(1..5, [5, 4, 3, 1, 2]);\n" *
          "c = array1d(1..5, [3, 4, 5, 1, 2]);\n" *
          "d = array1d(1..5, [2, 4, 3, 5, 1]);\n" *
          "k = array1d(1..5, [3, 1, 2, 5, 4]);\n" *
          "s = array1d(1..5, [3, 5, 2, 1, 4]);\n" *
          "\n" *
          "----------\n"
    return
end

function TODO_test_basic_fzn()
    model = FlatZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    @test MOI.supports_add_constrained_variable(model, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        OI.ScalarAffineFunction{Int},
        MOI.LessThan{Int},
    )
    # x ∈ {1, 2, 3}
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, -1 * x, MOI.LessThan(-1))
    c2 = MOI.add_constraint(model, 1 * x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x) ∈ Set([1, 2, 3])
    @test MOI.get(model, MOI.VariablePrimal(1), x) ∈ Set([1, 2, 3])
    return
end

function TODO_test_infeasible_fzn()
    model = FlatZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    @test MOI.supports_add_constrained_variable(model, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.LessThan{Int},
    )
    # x ∈ ∅
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, -1 * x, MOI.LessThan(-5))
    c2 = MOI.add_constraint(model, 1 * x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.INFEASIBLE
    @test MOI.get(model, MOI.ResultCount()) == 0
    return
end

end

TestFlatZinc.runtests()
