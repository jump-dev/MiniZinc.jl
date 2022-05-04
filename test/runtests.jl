# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# import Pkg
# Pkg.pkg"add MathOptInterface#od/cpsat-countgt"

module TestMiniZinc

using Test
import Chuffed_jll
import MathOptInterface
import MiniZinc

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

function test_write_bool()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    @test sprint(write, model) == """
    var bool: x1;
    solve satisfy;
    """
    return
end

function test_write_bool_false()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.add_constraint(model, x, MOI.EqualTo(0))
    @test sprint(write, model) == """
    var bool: x1 = false;
    solve satisfy;
    """
    return
end

function test_write_bool_true()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.add_constraint(model, x, MOI.EqualTo(1))
    @test sprint(write, model) == """
    var bool: x1 = true;
    solve satisfy;
    """
    return
end

function test_write_int()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.GreaterThan(1))
    MOI.add_constraint(model, x, MOI.LessThan(3))
    @test sprint(write, model) == """
    var 1 .. 3: x1;
    solve satisfy;
    """
    return
end

function test_write_interval()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1, 3))
    @test sprint(write, model) == """
    var 1 .. 3: x1;
    solve satisfy;
    """
    return
end

function test_write_greaterthan()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.GreaterThan(1))
    @test sprint(write, model) == """
    var int: x1;
    constraint int_gt(x1, 1);
    solve satisfy;
    """
    return
end

function test_write_lessthan()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.LessThan(2))
    @test sprint(write, model) == """
    var int: x1;
    constraint int_lt(x1, 2);
    solve satisfy;
    """
    return
end

function test_write_equalto()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.EqualTo(2))
    @test sprint(write, model) == """
    var int: x1 = 2;
    solve satisfy;
    """
    return
end

function test_write_float()
    model = MiniZinc.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    MOI.add_constraint(model, x, MOI.LessThan(3.0))
    @test sprint(write, model) == """
    var 1.0 .. 3.0: x1;
    solve satisfy;
    """
    return
end

function test_write_float_interval()
    model = MiniZinc.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.Interval(1.0, 3.0))
    @test sprint(write, model) == """
    var 1.0 .. 3.0: x1;
    solve satisfy;
    """
    return
end

function test_write_float_greaterthan()
    model = MiniZinc.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    @test sprint(write, model) == """
    var float: x1;
    constraint x1 >= 1.0;
    solve satisfy;
    """
    return
end

function test_write_float_lessthan()
    model = MiniZinc.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(2.0))
    @test sprint(write, model) == """
    var float: x1;
    constraint x1 <= 2.0;
    solve satisfy;
    """
    return
end

function test_write_float_equalto()
    model = MiniZinc.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.EqualTo(2.0))
    @test sprint(write, model) == """
    var float: x1 = 2.0;
    solve satisfy;
    """
    return
end

function test_write_int()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.GreaterThan(1))
    MOI.add_constraint(model, x, MOI.LessThan(3))
    @test sprint(write, model) == """
    var 1 .. 3: x1;
    solve satisfy;
    """
    return
end

function test_write_linear_eq()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    f = 1 * x + 2 * y + 4
    MOI.add_constraint(model, f, MOI.EqualTo(3))
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    constraint 1*x + 2*y = -1;
    solve satisfy;
    """
    return
end

function test_write_linear_lt()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    f = 1 * x + 2 * y + 4
    MOI.add_constraint(model, f, MOI.LessThan(3))
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    constraint 1*x + 2*y <= -1;
    solve satisfy;
    """
    return
end

function test_write_linear_gt()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    f = 1 * x + 2 * y + 4
    MOI.add_constraint(model, f, MOI.GreaterThan(3))
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    constraint 1*x + 2*y >= -1;
    solve satisfy;
    """
    return
end

function test_write_minimize()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    @test sprint(write, model) == """
    var int: x;
    solve minimize x;
    """
    return
end

function test_write_maximize()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    @test sprint(write, model) == """
    var int: x;
    solve maximize x;
    """
    return
end

function test_write_minimize_linear()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = 1 * x + 2 * y + 4
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    solve minimize 1*x + 2*y + 4;
    """
    return
end

function test_write_maximize_linear()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = 1 * x + 2 * y + 4
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    solve maximize 1*x + 2*y + 4;
    """
    return
end

function test_write_alldifferent()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    z = [x, y]
    MOI.add_constraint.(model, z, MOI.GreaterThan(1))
    MOI.add_constraint.(model, z, MOI.LessThan(3))
    MOI.add_constraint(model, MOI.VectorOfVariables(z), MOI.AllDifferent(2))
    @test sprint(write, model) == """
    include "alldifferent.mzn";
    var 1 .. 3: x;
    var 1 .. 3: y;
    constraint alldifferent([x, y]);
    solve satisfy;
    """
    return
end

function _test_chuffed_asset(file, args...)
    filename = joinpath(@__DIR__, "assets", file)
    ret = MiniZinc.run_flatzinc(Chuffed_jll.fznchuffed, filename, args...)
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

end

TestMiniZinc.runtests()
