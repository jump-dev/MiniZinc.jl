# Copyright (c) 2022 FlatZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

import Pkg
Pkg.pkg"add MathOptInterface#od/cpsat-countgt"

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

function test_basic_fzn()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(1))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = FlatZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    index_map, _ = MOI.optimize!(solver, model)
    @test_broken MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test_broken MOI.get(solver, MOI.ResultCount()) >= 1
    @test_broken(
        MOI.get(solver, MOI.VariablePrimal(), index_map[x]) in [1, 2, 3]
    )
    return
end

function test_infeasible_fzn()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(5))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = FlatZinc.Optimizer{Int}(Chuffed_jll.fznchuffed)
    _, _ = MOI.optimize!(solver, model)
    @test_broken MOI.get(solver, MOI.TerminationStatus()) === MOI.INFEASIBLE
    @test_broken MOI.get(solver, MOI.ResultCount()) == 0
    return
end

end

TestFlatZinc.runtests()
