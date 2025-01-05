# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMiniZinc

using Test
import Chuffed_jll
import MathOptInterface as MOI
import MiniZinc

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

function _test_file_contents(filename, args...)
    contents = read(filename, String)
    for arg in args
        @test occursin(arg, contents)
    end
    return
end

function test_write_bool_model()
    model = MiniZinc.Model{Bool}()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(true))
    @test sprint(write, model) == """
    var bool: x1;
    constraint bool_eq(x1, true);
    solve satisfy;
    """
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
    var bool: x1;
    constraint bool_eq(x1, false);
    solve satisfy;
    """
    return
end

function test_write_bool_true()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.add_constraint(model, x, MOI.EqualTo(1))
    @test sprint(write, model) == """
    var bool: x1;
    constraint bool_eq(x1, true);
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

function test_write_int_float_interval()
    model = MiniZinc.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1.5, 3.2))
    @test sprint(write, model) == """
    var 2 .. 3: x1;
    solve satisfy;
    """
    return
end

function test_write_int_float_greater_than()
    model = MiniZinc.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.GreaterThan(1.5))
    @test sprint(write, model) ==
          "var int: x1;\nconstraint int_le(2, x1);\nsolve satisfy;\n"
    return
end

function test_write_int_float_less_than()
    model = MiniZinc.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.LessThan(1.5))
    @test sprint(write, model) ==
          "var int: x1;\nconstraint int_le(x1, 1);\nsolve satisfy;\n"
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
    constraint int_le(1, x1);
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
    constraint int_le(x1, 2);
    solve satisfy;
    """
    return
end

function test_write_equalto()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.EqualTo(2))
    @test sprint(write, model) == """
    var 2 .. 2: x1;
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
    var 2.0 .. 2.0: x1;
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

function test_write_linear_eq_reified()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), z, "z")
    f = MOI.Utilities.operate(vcat, Int, z, 1 * x + 2 * y + 4)
    MOI.add_constraint(model, f, MOI.Reified(MOI.EqualTo(3)))
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    var bool: z;
    constraint z <-> 1*x + 2*y = -1;
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

function test_write_linear_lt_reified()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), z, "z")
    f = MOI.Utilities.operate(vcat, Int, z, 1 * x + 2 * y + 4)
    MOI.add_constraint(model, f, MOI.Reified(MOI.LessThan(3)))
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    var bool: z;
    constraint z <-> 1*x + 2*y <= -1;
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

function test_write_linear_gt_reified()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), z, "z")
    f = MOI.Utilities.operate(vcat, Int, z, 1 * x + 2 * y + 4)
    MOI.add_constraint(model, f, MOI.Reified(MOI.GreaterThan(3)))
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    var bool: z;
    constraint z <-> 1*x + 2*y >= -1;
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

function test_write_quadratic_objective()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = 1 * x * x + 2 * x * y + y + 4
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    solve minimize 1*x*x + 2*x*y + 1*y + 4;
    """
    return
end

function test_write_nonlinear_objective()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f1 = MOI.ScalarNonlinearFunction(:abs, Any[x])
    f2 = MOI.ScalarNonlinearFunction(:*, Any[f1, y])
    MOI.set(model, MOI.ObjectiveFunction{typeof(f2)}(), f2)
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    solve minimize (abs(x) * y);
    """
    return
end

function test_write_alldifferent()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.add_constraint.(model, [x, y], MOI.GreaterThan(1))
    MOI.add_constraint.(model, [x, y], MOI.LessThan(3))
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x, y]),
        MOI.AllDifferent(2),
    )
    @test sprint(write, model) == """
    var 1 .. 3: x;
    var 1 .. 3: y;
    constraint alldifferent([x, y]);
    solve satisfy;
    include "alldifferent.mzn";
    """
    return
end

function test_write_alldifferent_reified()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), z, "z")
    MOI.add_constraint.(model, [x, y], MOI.GreaterThan(1))
    MOI.add_constraint.(model, [x, y], MOI.LessThan(3))
    vv = MOI.VectorOfVariables([z; x; y])
    MOI.add_constraint(model, vv, MOI.Reified(MOI.AllDifferent(2)))
    @test sprint(write, model) == """
    var 1 .. 3: x;
    var 1 .. 3: y;
    var bool: z;
    constraint z <-> alldifferent([x, y]);
    solve satisfy;
    include "alldifferent.mzn";
    """
    return
end

function test_write_nonlinear_alldifferent()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    xy = [x, y]
    MOI.add_constraint.(model, xy, MOI.GreaterThan(1))
    MOI.add_constraint.(model, xy, MOI.LessThan(3))
    f = MOI.ScalarNonlinearFunction(:alldifferent, Any[xy])
    MOI.add_constraint(model, f, MOI.EqualTo(1))
    @test sprint(write, model) == """
    var 1 .. 3: x;
    var 1 .. 3: y;
    constraint alldifferent([x, y]) = 1;
    solve satisfy;
    include "alldifferent.mzn";
    """
    return
end

function test_write_nonlinear_alldifferent_reified()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), z, "z")
    xy = [x, y]
    MOI.add_constraint.(model, xy, MOI.GreaterThan(1))
    MOI.add_constraint.(model, xy, MOI.LessThan(3))
    f1 = MOI.ScalarNonlinearFunction(:alldifferent, Any[xy])
    f2 = MOI.ScalarNonlinearFunction(:reified, Any[z, f1])
    MOI.add_constraint(model, f2, MOI.EqualTo(1))
    @test sprint(write, model) == """
    var 1 .. 3: x;
    var 1 .. 3: y;
    var bool: z;
    constraint (z <-> alldifferent([x, y])) = 1;
    solve satisfy;
    include "alldifferent.mzn";
    """
    return
end

function test_write_countdistinct()
    model = MiniZinc.Model{Int}()
    y = [MOI.add_constrained_variable(model, MOI.Integer()) for _ in 1:4]
    x = first.(y)
    MOI.add_constraint.(model, x, MOI.Interval(1, 4))
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.CountDistinct(4))
    for i in 1:4
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    @test sprint(write, model) == """
    var 1 .. 4: x1;
    var 1 .. 4: x2;
    var 1 .. 4: x3;
    var 1 .. 4: x4;
    constraint nvalue(x1, [x2, x3, x4]);
    solve satisfy;
    include "nvalue.mzn";
    """
    return
end

function test_write_countdistinct_reified()
    model = MiniZinc.Model{Int}()
    y = [MOI.add_constrained_variable(model, MOI.Integer()) for _ in 1:4]
    x = first.(y)
    MOI.add_constraint.(model, x, MOI.Interval(1, 4))
    b, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), b, "b")
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([b; x]),
        MOI.Reified(MOI.CountDistinct(4)),
    )
    for i in 1:4
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    @test sprint(write, model) == """
    var 1 .. 4: x1;
    var 1 .. 4: x2;
    var 1 .. 4: x3;
    var 1 .. 4: x4;
    var bool: b;
    constraint b <-> nvalue(x1, [x2, x3, x4]);
    solve satisfy;
    include "nvalue.mzn";
    """
    return
end

function test_write_countbelongs()
    model = MiniZinc.Model{Int}()
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:4]
    for i in 1:4
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    set = Set([3, 4])
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.CountBelongs(4, set),
    )
    @test sprint(write, model) == """
    var int: x1;
    var int: x2;
    var int: x3;
    var int: x4;
    constraint among(x1, [x2, x3, x4], {3, 4});
    solve satisfy;
    include "among.mzn";
    """
    return
end

function test_write_countbelongs_reified()
    model = MiniZinc.Model{Int}()
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:4]
    for i in 1:4
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    b, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), b, "b")
    set = Set([3, 4])
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([b; x]),
        MOI.Reified(MOI.CountBelongs(4, set)),
    )
    @test sprint(write, model) == """
    var int: x1;
    var int: x2;
    var int: x3;
    var int: x4;
    var bool: b;
    constraint b <-> among(x1, [x2, x3, x4], {3, 4});
    solve satisfy;
    include "among.mzn";
    """
    return
end

function test_write_countatleast()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    z, _ = MOI.add_constrained_variable(model, MOI.Integer())
    variables = [x, y, y, z]
    partitions = [2, 2]
    set = Set([3])
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(variables),
        MOI.CountAtLeast(1, partitions, set),
    )
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(model, MOI.VariableName(), z, "z")
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    var int: z;
    constraint at_least(1, [{x, y}, {y, z}], {3});
    solve satisfy;
    include "at_least.mzn";
    """
    return
end

function test_write_countatleast_reified()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    z, _ = MOI.add_constrained_variable(model, MOI.Integer())
    b, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    variables = [b, x, y, y, z]
    partitions = [2, 2]
    set = Set([3])
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(variables),
        MOI.Reified(MOI.CountAtLeast(1, partitions, set)),
    )
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(model, MOI.VariableName(), z, "z")
    MOI.set(model, MOI.VariableName(), b, "b")
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    var int: z;
    var bool: b;
    constraint b <-> at_least(1, [{x, y}, {y, z}], {3});
    solve satisfy;
    include "at_least.mzn";
    """
    return
end

function test_write_countgreaterthan()
    model = MiniZinc.Model{Int}()
    c, _ = MOI.add_constrained_variable(model, MOI.Integer())
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([c; y; x]),
        MOI.CountGreaterThan(5),
    )
    MOI.set(model, MOI.VariableName(), c, "c")
    MOI.set(model, MOI.VariableName(), y, "y")
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    @test sprint(write, model) == """
    var int: c;
    var int: y;
    var int: x1;
    var int: x2;
    var int: x3;
    constraint count_gt([x1, x2, x3], y, c);
    solve satisfy;
    include "count_gt.mzn";
    """
    return
end

function test_write_countgreaterthan_reified()
    model = MiniZinc.Model{Int}()
    c, _ = MOI.add_constrained_variable(model, MOI.Integer())
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    b, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), b, "b")
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([b; c; y; x]),
        MOI.Reified(MOI.CountGreaterThan(5)),
    )
    MOI.set(model, MOI.VariableName(), c, "c")
    MOI.set(model, MOI.VariableName(), y, "y")
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    @test sprint(write, model) == """
    var int: c;
    var int: y;
    var int: x1;
    var int: x2;
    var int: x3;
    var bool: b;
    constraint b <-> count_gt([x1, x2, x3], y, c);
    solve satisfy;
    include "count_gt.mzn";
    """
    return
end

function test_write_binpacking()
    model = MiniZinc.Model{Int}()
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:2]
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.BinPacking(2, [3, 4]),
    )
    MOI.set(model, MOI.VariableName(), x[1], "x1")
    MOI.set(model, MOI.VariableName(), x[2], "x2")
    @test sprint(write, model) == """
    var int: x1;
    var int: x2;
    constraint bin_packing(2, [x1, x2], [3, 4]);
    solve satisfy;
    include "bin_packing.mzn";
    """
    return
end

function test_write_binpacking_reified()
    model = MiniZinc.Model{Int}()
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:2]
    b, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([b; x]),
        MOI.Reified(MOI.BinPacking(2, [3, 4])),
    )
    MOI.set(model, MOI.VariableName(), x[1], "x1")
    MOI.set(model, MOI.VariableName(), x[2], "x2")
    MOI.set(model, MOI.VariableName(), b, "b")
    @test sprint(write, model) == """
    var int: x1;
    var int: x2;
    var bool: b;
    constraint b <-> bin_packing(2, [x1, x2], [3, 4]);
    solve satisfy;
    include "bin_packing.mzn";
    """
    return
end

function test_write_path()
    model = MiniZinc.Model{Int}()
    from = [1, 1, 2, 2, 3]
    to = [2, 3, 3, 4, 4]
    s, _ = MOI.add_constrained_variable(model, MOI.Integer())
    t, _ = MOI.add_constrained_variable(model, MOI.Integer())
    N, E = 4, 5
    ns = MOI.add_variables(model, N)
    MOI.add_constraint.(model, ns, MOI.ZeroOne())
    es = MOI.add_variables(model, E)
    MOI.add_constraint.(model, es, MOI.ZeroOne())
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([s; t; ns; es]),
        MOI.Path(from, to),
    )
    MOI.set(model, MOI.VariableName(), s, "s")
    MOI.set(model, MOI.VariableName(), t, "t")
    for i in 1:N
        MOI.set(model, MOI.VariableName(), ns[i], "ns$i")
    end
    for i in 1:E
        MOI.set(model, MOI.VariableName(), es[i], "es$i")
    end
    @test sprint(write, model) == """
    var int: s;
    var int: t;
    var bool: ns1;
    var bool: ns2;
    var bool: ns3;
    var bool: ns4;
    var bool: es1;
    var bool: es2;
    var bool: es3;
    var bool: es4;
    var bool: es5;
    constraint path(4, 5, [1, 1, 2, 2, 3], [2, 3, 3, 4, 4], s, t, [ns1, ns2, ns3, ns4], [es1, es2, es3, es4, es5]);
    solve satisfy;
    include "path.mzn";
    """
    return
end

function test_write_cumulative()
    model = MiniZinc.Model{Int}()
    s = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    d = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    r = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    b, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([s; d; r; b]),
        MOI.Cumulative(10),
    )
    MOI.set(model, MOI.VariableName(), b, "b")
    for i in 1:3
        MOI.set(model, MOI.VariableName(), s[i], "s$i")
        MOI.set(model, MOI.VariableName(), d[i], "d$i")
        MOI.set(model, MOI.VariableName(), r[i], "r$i")
    end
    @test sprint(write, model) == """
    var int: s1;
    var int: s2;
    var int: s3;
    var int: d1;
    var int: d2;
    var int: d3;
    var int: r1;
    var int: r2;
    var int: r3;
    var int: b;
    constraint cumulative([s1, s2, s3], [d1, d2, d3], [r1, r2, r3], b);
    solve satisfy;
    include "cumulative.mzn";
    """
    return
end

function test_write_cumulative_reified()
    model = MiniZinc.Model{Int}()
    s = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    d = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    r = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    b, _ = MOI.add_constrained_variable(model, MOI.Integer())
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([z; s; d; r; b]),
        MOI.Reified(MOI.Cumulative(10)),
    )
    MOI.set(model, MOI.VariableName(), b, "b")
    MOI.set(model, MOI.VariableName(), z, "z")
    for i in 1:3
        MOI.set(model, MOI.VariableName(), s[i], "s$i")
        MOI.set(model, MOI.VariableName(), d[i], "d$i")
        MOI.set(model, MOI.VariableName(), r[i], "r$i")
    end
    @test sprint(write, model) == """
    var int: s1;
    var int: s2;
    var int: s3;
    var int: d1;
    var int: d2;
    var int: d3;
    var int: r1;
    var int: r2;
    var int: r3;
    var int: b;
    var bool: z;
    constraint z <-> cumulative([s1, s2, s3], [d1, d2, d3], [r1, r2, r3], b);
    solve satisfy;
    include "cumulative.mzn";
    """
    return
end

function test_write_table()
    model = MiniZinc.Model{Int}()
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    table = [1 1 0; 0 1 1]
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Table(table))
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    @test sprint(write, model) == """
    var int: x1;
    var int: x2;
    var int: x3;
    constraint table([x1, x2, x3], [| 1, 1, 0 | 0, 1, 1 |]);
    solve satisfy;
    include "table.mzn";
    """
    return
end

function test_write_table_reified()
    model = MiniZinc.Model{Int}()
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    table = [1 1 0; 0 1 1]
    b, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), b, "b")
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([b; x]),
        MOI.Reified(MOI.Table(table)),
    )
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    @test sprint(write, model) == """
    var int: x1;
    var int: x2;
    var int: x3;
    var bool: b;
    constraint b <-> table([x1, x2, x3], [| 1, 1, 0 | 0, 1, 1 |]);
    solve satisfy;
    include "table.mzn";
    """
    return
end

function test_write_nonlinear_ifelse()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    f1 = MOI.ScalarNonlinearFunction(:>, Any[x, 0])
    f = MOI.ScalarNonlinearFunction(:ifelse, Any[f1, x, y])
    MOI.add_constraint(model, f, MOI.EqualTo(1))
    @test sprint(write, model) == """
    var int: x;
    var int: y;
    constraint (if (x > 0) then x else y endif) = 1;
    solve satisfy;
    """
    return
end

function test_model_unsupported_vectoraffine_constraint()
    model = MiniZinc.Model{Int}()
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, Int, x[1], 2 * x[2])
    @test !MOI.supports_constraint(model, typeof(f), MOI.AllDifferent)
    set = MOI.Reified(MOI.GreaterThan(1))
    @test MOI.supports_constraint(model, typeof(f), typeof(set))
    return
end

function test_reified_dimension()
    @test MOI.dimension(MOI.Reified(MOI.GreaterThan(1))) == 2
    @test MOI.dimension(MOI.Reified(MOI.AllDifferent(2))) == 3
    return
end

function test_write_circuit()
    model = MiniZinc.Model{Int}()
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Circuit(3))
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$i")
    end
    @test sprint(write, model) == """
    var int: x1;
    var int: x2;
    var int: x3;
    constraint circuit([x1, x2, x3]);
    solve satisfy;
    include "circuit.mzn";
    """
    return
end

function test_write_bool_or_true()
    model = MiniZinc.Model{Bool}()
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.VariableName(), x, ["x1", "x2"])
    MOI.add_constraint(
        model,
        MOI.ScalarNonlinearFunction(:||, Any[x[1], x[2]]),
        MOI.EqualTo{Bool}(true),
    )
    @test sprint(write, model) == """
    var bool: x1;
    var bool: x2;
    constraint (x1 \\/ x2) = true;
    solve satisfy;
    """
    return
end

function test_write_bool_and_false()
    model = MiniZinc.Model{Bool}()
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.VariableName(), x, ["x1", "x2"])
    MOI.add_constraint(
        model,
        MOI.ScalarNonlinearFunction(:&&, Any[x[1], x[2]]),
        MOI.EqualTo{Bool}(false),
    )
    @test sprint(write, model) == """
    var bool: x1;
    var bool: x2;
    constraint (x1 /\\ x2) = false;
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
    solution = "x = 10;\n\n----------\n==========\n"
    @test startswith(_test_chuffed_asset("one_solution.fzn"), solution)
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

function test_moi_basic_fzn()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(1))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = MiniZinc.Optimizer{Int}("chuffed")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) in [1, 2, 3]
    @test MOI.get(solver, MOI.RawStatusString()) == "SATISFIABLE"
    return
end

function test_moi_support_solution_limit()
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.supports(solver, MOI.SolutionLimit())
    attr = MOI.RawOptimizerAttribute("num_solutions")
    MOI.supports(solver, attr)
    @test MOI.get(solver, MOI.SolutionLimit()) === nothing
    @test MOI.get(solver, attr) === nothing
    MOI.set(solver, MOI.SolutionLimit(), 100)
    @test MOI.get(solver, attr) == 100
    @test MOI.get(solver, MOI.SolutionLimit()) == 100
    MOI.set(solver, attr, 100)
    @test MOI.get(solver, attr) == 100
    @test MOI.get(solver, MOI.SolutionLimit()) == 100
    MOI.set(solver, MOI.SolutionLimit(), nothing)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(solver, MOI.SolutionLimit(), -1),
    )
    @test MOI.get(solver, MOI.SolutionLimit()) === nothing
    @test MOI.get(solver, attr) === nothing
    return
end

function test_moi_var_domain_infeasible_fzn()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(5))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.INFEASIBLE
    @test MOI.get(solver, MOI.ResultCount()) == 0
    @test MOI.get(solver, MOI.RawStatusString()) == "UNSATISFIABLE"
    return
end

function test_moi_infeasible_fzn()
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(model, 3)
    MOI.add_constraint.(model, x, MOI.Integer())
    MOI.add_constraint.(model, x, MOI.GreaterThan(1))
    MOI.add_constraint(model, sum(x, init = 0), MOI.LessThan(2))
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.INFEASIBLE
    @test MOI.get(solver, MOI.ResultCount()) == 0
    return
end

function test_moi_one_solution_fzn()
    model = MOI.Utilities.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1, 10))
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    solver = MiniZinc.Optimizer{Int}("chuffed")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) == 10
    @test MOI.get(solver, MOI.ObjectiveValue()) == 10
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    solver = MiniZinc.Optimizer{Int}("chuffed")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) == 1
    @test MOI.get(solver, MOI.ObjectiveValue()) == 1
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
    solver = MiniZinc.Optimizer{Int}("chuffed")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    v = [MOI.get(solver, MOI.VariablePrimal(), index_map[xi]) for xi in x]
    @test all(v .>= 0)
    @test all(v .<= 1)
    @test sum(v) == 2
    return
end

function test_moi_tests()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Int}(),
        MiniZinc.Optimizer{Int}("highs"),
    )
    config = MOI.Test.Config(Int)
    MOI.Test.runtests(model, config, include = String["test_cpsat_"])
    return
end

function test_model_filename()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x1")
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(1))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = MiniZinc.Optimizer{Int}("chuffed")
    attr = MOI.RawOptimizerAttribute("model_filename")
    @test MOI.supports(solver, attr)
    @test MOI.get(solver, attr) == ""
    MOI.set(solver, attr, "test.mzn")
    @test MOI.get(solver, attr) == "test.mzn"
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) in [1, 2, 3]
    _test_file_contents("test.mzn", "var 1 .. 3: x1;\n", "solve satisfy;\n")
    rm("test.mzn")
    return
end

function test_model_nonlinear_boolean()
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(model, 2)
    MOI.set.(model, MOI.VariableName(), x, ["x1", "x2"])
    MOI.add_constraint.(model, x, MOI.ZeroOne())
    for (f, c) in [(:||, 1), (:&&, 0)]
        snf = MOI.ScalarNonlinearFunction(f, Any[x...])
        MOI.add_constraint(model, snf, MOI.EqualTo(c))
    end
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    y = [index_map[v] for v in x]
    sol = round.(Bool, MOI.get(solver, MOI.VariablePrimal(), y))
    @test (sol[1] || sol[2])
    @test !(sol[1] && sol[2])
    _test_file_contents(
        "test.mzn",
        "var bool: x1;\n",
        "var bool: x2;\n",
        "constraint (x1 \\/ x2) = 1;\n",
        "constraint (x1 /\\ x2) = 0;\n",
        "solve satisfy;\n",
    )
    rm("test.mzn")
    return
end

function test_model_nonlinear_boolean_nested()
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(model, 2)
    MOI.set.(model, MOI.VariableName(), x, ["x2", "x3"])
    MOI.add_constraint.(model, x, MOI.ZeroOne())
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), y, "x1")
    MOI.add_constraint(model, y, MOI.Integer())
    MOI.add_constraint(model, y, MOI.Interval(0, 10))
    SNF(f::Symbol, args...) = MOI.ScalarNonlinearFunction(f, Any[args...])
    # x[1] || (x[2] && (y < 5))
    snf = SNF(:||, x[1], SNF(:&&, x[2], SNF(:<, y, 5)))
    MOI.add_constraint(model, snf, MOI.GreaterThan(1))
    MOI.add_constraint(model, SNF(:<, x[1], 1), MOI.EqualTo(1))
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    sol_x = [index_map[v] for v in [x; y]]
    sol = round.(Int, MOI.get(solver, MOI.VariablePrimal(), sol_x))
    @test sol[1] == 0
    @test sol[2] == 1
    @test sol[3] < 5
    _test_file_contents(
        "test.mzn",
        "var 0 .. 10: x1;\n",
        "var bool: x2;\n",
        "var bool: x3;\n",
        "constraint (x2 < 1) = 1;\n",
        "constraint (x2 \\/ (x3 /\\ (x1 < 5))) >= 1;\n",
        "solve satisfy;\n",
    )
    rm("test.mzn")
    return
end

function test_model_nonlinear_boolean_jump()
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(model, 2)
    MOI.set.(model, MOI.VariableName(), x, ["x1", "x2"])
    MOI.add_constraint.(model, x, MOI.ZeroOne())
    for (f, c) in [(:||, 1), (:&&, 0)]
        snf1 = MOI.ScalarNonlinearFunction(f, Any[x...])
        snf2 = MOI.ScalarNonlinearFunction(:-, Any[snf1, c])
        MOI.add_constraint(model, snf2, MOI.EqualTo(0))
    end
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    y = [index_map[v] for v in x]
    sol = round.(Bool, MOI.get(solver, MOI.VariablePrimal(), y))
    @test (sol[1] || sol[2])
    @test !(sol[1] && sol[2])
    _test_file_contents(
        "test.mzn",
        "var bool: x1;\n",
        "var bool: x2;\n",
        "constraint ((x1 \\/ x2) - 1) = 0;\n",
        "constraint ((x1 /\\ x2) - 0) = 0;\n",
        "solve satisfy;\n",
    )
    rm("test.mzn")
    return
end

function test_model_nonlinear_boolean_nested_not()
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(model, 3)
    MOI.set.(model, MOI.VariableName(), x, ["x1", "x2", "x3"])
    MOI.add_constraint.(model, x, MOI.ZeroOne())
    # x1 => !(x2 ⊻ x3)
    snf1 = MOI.ScalarNonlinearFunction(:⊻, Any[x[2], x[3]])
    snf2 = MOI.ScalarNonlinearFunction(:!, Any[snf1])
    snf3 = MOI.ScalarNonlinearFunction(:(=>), Any[x[1], snf2])
    MOI.add_constraint(model, snf3, MOI.EqualTo(1))
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    y = [index_map[v] for v in x]
    sol = round.(Bool, MOI.get(solver, MOI.VariablePrimal(), y))
    @test ifelse(sol[1], !(sol[2] || sol[3]), true)
    _test_file_contents(
        "test.mzn",
        "var bool: x1;\n",
        "var bool: x2;\n",
        "var bool: x3;\n",
        "constraint (x1 -> not((x2 xor x3))) = 1;\n",
        "solve satisfy;\n",
    )
    rm("test.mzn")
    return
end

function test_model_nonlinear_bool_model()
    model = MOI.Utilities.Model{Bool}()
    x = MOI.add_variables(model, 3)
    MOI.set.(model, MOI.VariableName(), x, ["x1", "x2", "x3"])
    # x1 <--> !(x2 <-- x3)
    snf1 = MOI.ScalarNonlinearFunction(:(<--), Any[x[2], x[3]])
    snf2 = MOI.ScalarNonlinearFunction(:!, Any[snf1])
    snf3 = MOI.ScalarNonlinearFunction(:(<-->), Any[x[1], snf2])
    MOI.add_constraint(model, snf3, MOI.EqualTo(true))
    solver = MiniZinc.Optimizer{Bool}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    y = [index_map[v] for v in x]
    sol = MOI.get(solver, MOI.VariablePrimal(), y)
    @test sol[1] == !ifelse(sol[3], sol[2], true)
    _test_file_contents(
        "test.mzn",
        "var bool: x1;\n",
        "var bool: x2;\n",
        "var bool: x3;\n",
        "constraint (x1 <-> not((x2 <- x3))) = true;\n",
        "solve satisfy;\n",
    )
    rm("test.mzn")
    return
end

function test_model_nonlinear_bool_vector_arg()
    model = MOI.Utilities.Model{Int}()
    x1 = MOI.add_variables(model, 3)
    MOI.add_constraint.(model, x1, MOI.ZeroOne())
    MOI.set.(model, MOI.VariableName(), x1, ["x1", "x2", "x3"])
    x2 = MOI.add_variables(model, 3)
    MOI.add_constraint.(model, x2, MOI.ZeroOne())
    MOI.set.(model, MOI.VariableName(), x2, ["x4", "x5", "x6"])
    # forall([exists(x1), exists(x2)])
    snf1 = MOI.ScalarNonlinearFunction(:exists, Any[x1])
    snf2 = MOI.ScalarNonlinearFunction(:exists, Any[x2])
    snf3 = MOI.ScalarNonlinearFunction(:forall, Any[[snf1, snf2]])
    MOI.add_constraint(model, snf3, MOI.EqualTo(1))
    # count([x1..., x2...]) = 2
    snf = MOI.ScalarNonlinearFunction(:count, Any[[x1..., x2...]])
    MOI.add_constraint(model, snf, MOI.EqualTo(2))
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    y1 = MOI.get(solver, MOI.VariablePrimal(), [index_map[v] for v in x1])
    y2 = MOI.get(solver, MOI.VariablePrimal(), [index_map[v] for v in x2])
    @test sum(y1) == 1
    @test sum(y2) == 1
    _test_file_contents(
        "test.mzn",
        "var bool: x1;\n",
        "var bool: x2;\n",
        "var bool: x3;\n",
        "var bool: x4;\n",
        "var bool: x5;\n",
        "var bool: x6;\n",
        "constraint forall([exists([x1, x2, x3]), exists([x4, x5, x6])]) = 1;\n",
        "constraint count([x1, x2, x3, x4, x5, x6]) = 2;\n",
        "solve satisfy;\n",
    )
    rm("test.mzn")
    return
end

function test_model_nonlinear_alldifferent_reified()
    model = MiniZinc.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), x, "x")
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.set(model, MOI.VariableName(), y, "y")
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    MOI.set(model, MOI.VariableName(), z, "z")
    xy = [x, y]
    MOI.add_constraint.(model, xy, MOI.GreaterThan(1))
    MOI.add_constraint.(model, xy, MOI.LessThan(3))
    f1 = MOI.ScalarNonlinearFunction(:alldifferent, Any[xy])
    f2 = MOI.ScalarNonlinearFunction(:reified, Any[z, f1])
    MOI.add_constraint(model, f2, MOI.EqualTo(1))
    MOI.add_constraint(model, z, MOI.EqualTo(0))
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    xy_sol = MOI.get(solver, MOI.VariablePrimal(), [index_map[v] for v in xy])
    z_sol = MOI.get(solver, MOI.VariablePrimal(), index_map[z])
    @test !allunique(xy_sol)
    @test iszero(z_sol)
    _test_file_contents(
        "test.mzn",
        "var 1 .. 3: x;\n",
        "var 1 .. 3: y;\n",
        "var bool: z;\n",
        "constraint bool_eq(z, false);\n",
        "constraint (z <-> alldifferent([x, y])) = 1;\n",
        "solve satisfy;\n",
        "include \"alldifferent.mzn\";\n",
    )
    rm("test.mzn")
    return
end

function test_unsupported_nonlinear_operator()
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variable(model)
    snf = MOI.ScalarNonlinearFunction(:my_f, Any[x])
    MOI.add_constraint(model, snf, MOI.EqualTo(1))
    solver = MiniZinc.Optimizer{Int}("chuffed")
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    @test_throws(
        MOI.UnsupportedNonlinearOperator(:my_f),
        MOI.optimize!(solver, model),
    )
    rm("test.mzn")
    return
end

function test_supported_operators()
    model = MiniZinc.Model{Int}()
    ops = MOI.get(model, MOI.ListOfSupportedNonlinearOperators())
    @test ops isa Vector{Symbol}
    @test :(!) in ops
    @test :|| in ops
    @test :* in ops
    @test :⊻ in ops
    @test :(<-->) in ops
    @test :forall in ops
    @test :count in ops
    @test :alldifferent in ops
    @test :reified in ops
    @test :ifelse in ops
    return
end

function test_model_solver_name()
    solver = MiniZinc.Optimizer{Int}("chuffed")
    @test MOI.get(solver, MOI.SolverName()) == "MiniZinc"
    return
end

function test_supports_vector_objective()
    model = MiniZinc.Model{Int}()
    @test !MOI.supports(model, MOI.ObjectiveFunction{MOI.VectorOfVariables}())
    return
end

function test_highs_feasibility()
    model = MOI.Utilities.Model{Float64}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3.0))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = MiniZinc.Optimizer{Float64}("highs")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) in [1.0, 2.0, 3.0]
    @test MOI.get(solver, MOI.RawStatusString()) == "SATISFIABLE"
    return
end

function test_highs_optimization()
    model = MOI.Utilities.Model{Int}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1, 10))
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    solver = MiniZinc.Optimizer{Int}("highs")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) == 10.0
    @test MOI.get(solver, MOI.ObjectiveValue()) == 10.0
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    solver = MiniZinc.Optimizer{Int}("highs")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) == 1.0
    @test MOI.get(solver, MOI.ObjectiveValue()) == 1.0
    @test MOI.get(solver, MOI.RawStatusString()) == "SATISFIABLE"
    return
end

function test_time_limit_sec()
    solver = MiniZinc.Optimizer{Int}("highs")
    @test MOI.supports(solver, MOI.TimeLimitSec())
    @test MOI.get(solver, MOI.TimeLimitSec()) === nothing
    MOI.set(solver, MOI.TimeLimitSec(), 1)
    @test MOI.get(solver, MOI.TimeLimitSec()) == 1.0
    MOI.set(solver, MOI.TimeLimitSec(), nothing)
    @test MOI.get(solver, MOI.TimeLimitSec()) === nothing
    return
end

function test_highs_optimization_time_limit()
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1.0, 10.0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    solver = MiniZinc.Optimizer{Float64}("highs")
    @test isnan(MOI.get(solver, MOI.SolveTimeSec()))
    MOI.set(solver, MOI.TimeLimitSec(), 9e-4) # Very small limit
    index_map, _ = MOI.optimize!(solver, model)
    @test !isnan(MOI.get(solver, MOI.SolveTimeSec()))
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.TIME_LIMIT
    solver = MiniZinc.Optimizer{Float64}("highs")
    MOI.set(solver, MOI.TimeLimitSec(), 100)
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    return
end

function test_version_number()
    solver = MiniZinc.Optimizer{Float64}("highs")
    version = MOI.get(solver, MOI.SolverVersion())
    @test version isa VersionNumber
    @test version >= v"2.7.4"
    return
end

function test_run_failure()
    model = MiniZinc.Optimizer{Float64}("a")
    MOI.optimize!(model, MOI.Utilities.Model{Float64}())
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OTHER_ERROR
    @test occursin("=ERROR=", MOI.get(model, MOI.RawStatusString()))
    return
end

function test_highs_free_binary()
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    solver = MiniZinc.Optimizer{Float64}("highs")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) == MOI.OPTIMAL
    return
end

function test_highs_int_frac_lb()
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.GreaterThan(0.5))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    solver = MiniZinc.Optimizer{Float64}("highs")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) ≈ 1.0
    return
end

function test_highs_int_frac_ub()
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.LessThan(2.5))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    solver = MiniZinc.Optimizer{Float64}("highs")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) ≈ 2.0
    return
end

function test_infix_unary_minus()
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Interval(1.0, 3.0))
    solver = MiniZinc.Optimizer{Float64}("highs")
    f = MOI.ScalarNonlinearFunction(:-, Any[x])
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) ≈ 1.0
    @test MOI.get(solver, MOI.ObjectiveValue()) ≈ -1.0
    return
end

function test_infix_unary_addition()
    for op in (:+, :*)
        model = MOI.Utilities.Model{Float64}()
        x, _ = MOI.add_constrained_variable(model, MOI.Interval(1.0, 3.0))
        solver = MiniZinc.Optimizer{Float64}("highs")
        f = MOI.ScalarNonlinearFunction(op, Any[x])
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        index_map, _ = MOI.optimize!(solver, model)
        @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) ≈ 3.0
        @test MOI.get(solver, MOI.ObjectiveValue()) ≈ 3.0
    end
    return
end

function test_integer_bounds_interval()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(model, 3)
    MOI.add_constraint.(model, x, MOI.Integer())
    MOI.add_constraint.(model, x, MOI.Interval(1.0, 3.0))
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.AllDifferent(3))
    mzn = MiniZinc.Optimizer{Float64}("highs")
    index_map, _ = MOI.optimize!(mzn, model)
    @test MOI.get(mzn, MOI.TerminationStatus()) == MOI.OPTIMAL
    y = [MOI.get(mzn, MOI.VariablePrimal(), index_map[xi]) for xi in x]
    @test sort(y) == [1, 2, 3]
    return
end

function test_integer_bounds_greater_than()
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.GreaterThan(1.5))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    mzn = MiniZinc.Optimizer{Float64}("highs")
    index_map, _ = MOI.optimize!(mzn, model)
    @test MOI.get(mzn, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(mzn, MOI.VariablePrimal(), index_map[x]) == 2
    return
end

function test_integer_bounds_less_than_than()
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.LessThan(1.5))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    mzn = MiniZinc.Optimizer{Float64}("highs")
    index_map, _ = MOI.optimize!(mzn, model)
    @test MOI.get(mzn, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(mzn, MOI.VariablePrimal(), index_map[x]) == 1
end

function test_minizincset()
    set = MiniZinc.MiniZincSet("diffn", [1:2, 3:4, 5:6, 7:8])
    @test MOI.dimension(set) == 8
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x, _ = MOI.add_constrained_variables(model, set)
    @test length(x) == 8
    return
end

function test_example_louvain()
    edges = [(1, 2, 1); (1, 3, 4); (1, 5, 7); (2, 4, 10); (3, 5, 12)]
    m = sum(e[3] for e in edges)
    n_nodes = 5
    k = [
        sum((w for (u, v, w) in edges if (u == i || v == i)), init = 0) for
        i in 1:n_nodes
    ]
    n_communities = 2
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
    )
    MOI.set(
        model,
        MOI.RawOptimizerAttribute("model_filename"),
        "test_louvain.mzn",
    )
    x = MOI.add_variables(model, n_nodes)
    MOI.add_constraint(model, x[1], MOI.EqualTo(1))
    MOI.add_constraint.(model, x[2:end], MOI.Interval(1, n_communities))
    terms = Any[]
    for (u, v, w) in edges
        o = 2 * m * w - k[u] * k[v]
        f1 = MOI.ScalarNonlinearFunction(:(==), Any[x[u], x[v]])
        f2 = MOI.ScalarNonlinearFunction(:ifelse, Any[f1, o, 0])
        push!(terms, f2)
    end
    f = MOI.ScalarNonlinearFunction(:+, terms)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    x_sol = MOI.get(model, MOI.VariablePrimal(), x)
    @test x_sol == [1, 2, 1, 2, 1]
    @test MOI.get(model, MOI.ObjectiveValue()) == 1410
    @test MOI.Utilities.eval_variables(vi -> x_sol[vi.value], model, f) == 1410
    rm("test_louvain.mzn")
    return
end

function test_example_nqueens_alldiff()
    n = 4
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
        with_bridge_type = Int,
    )
    MOI.set(
        model,
        MOI.RawOptimizerAttribute("model_filename"),
        "test_nqueens.mzn",
    )
    q = MOI.add_variables(model, n)
    MOI.add_constraint.(model, q, MOI.Interval(1, n))
    MOI.add_constraint(model, MOI.VectorOfVariables(q), MOI.AllDifferent(n))
    for op in (+, -)
        f = MOI.Utilities.vectorize([op(q[i], i) for i in eachindex(q)])
        MOI.add_constraint(model, f, MOI.AllDifferent(n))
    end
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    q_sol = MOI.get(model, MOI.VariablePrimal(), q)
    @test allunique(q_sol)
    @test allunique(q_sol .+ (1:n))
    @test allunique(q_sol .- (1:n))
    rm("test_nqueens.mzn")
    return
end

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
    actual_count::Vector,
    termination_status = MOI.OPTIMAL,
)
    n = 8
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === termination_status
    res_count = MOI.get(model, MOI.ResultCount())
    @test res_count in actual_count
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

function test_example_nqueens_solve_num_solutions_100()
    model, q = _init_nqueens_solve_num_solutions()
    MOI.set(model, MOI.SolutionLimit(), 100)
    # This value is wrong due to a bug in Chuffed@0.13.2. If it is fixed in
    # the future, the true value should be 92.
    # See https://github.com/jump-dev/MiniZinc.jl/issues/84 for details.
    _test_nqueens_solve_num_solutions(model, q, [52, 92])
    return
end

function test_example_nqueens_solve_num_solutions_25()
    model, q = _init_nqueens_solve_num_solutions()
    MOI.set(model, MOI.SolutionLimit(), 25)
    _test_nqueens_solve_num_solutions(model, q, [25], MOI.SOLUTION_LIMIT)
    return
end

function test_example_nqueens_solve_num_solutions_not_set()
    model, q = _init_nqueens_solve_num_solutions()
    _test_nqueens_solve_num_solutions(model, q, [1])
    return
end

function test_example_nqueens_solve_num_solutions_1()
    model, q = _init_nqueens_solve_num_solutions()
    MOI.set(model, MOI.SolutionLimit(), 1)
    _test_nqueens_solve_num_solutions(model, q, [1], MOI.SOLUTION_LIMIT)
    return
end

function test_example_nqueens_num_solutions_throw()
    model, _ = _init_nqueens_solve_num_solutions()
    for value in (-1, 0, 1.1, "two")
        @test_throws(
            MOI.SetAttributeNotAllowed,
            MOI.set(model, MOI.SolutionLimit(), value)
        )
    end
    return
end

function test_example_nqueens_expr_tree()
    n = 4
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
    )
    MOI.set(
        model,
        MOI.RawOptimizerAttribute("model_filename"),
        "test_expr_tree.mzn",
    )
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
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    q_sol = MOI.get(model, MOI.VariablePrimal(), q)
    @test allunique(q_sol)
    @test allunique(q_sol .+ (1:n))
    @test allunique(q_sol .- (1:n))
    rm("test_expr_tree.mzn")
    return
end

# Inspired from the square packing tutorial in https://www.minizinc.org/
function test_example_packing()
    n = 6
    sizes = collect(1:n)
    upper_bound = sum(sizes)
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
        with_bridge_type = Int,
    )
    MOI.set(
        model,
        MOI.RawOptimizerAttribute("model_filename"),
        "test_packing.mzn",
    )
    # We need this `s` variable that is trivially equal to `sizes`
    # because `MiniZincSet` supports only VectorOfVariables
    s = [MOI.add_constrained_variable(model, MOI.Integer())[1] for i in 1:n]
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for i in 1:n]
    y = [MOI.add_constrained_variable(model, MOI.Integer())[1] for i in 1:n]
    max_x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    max_y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint.(model, s, MOI.EqualTo.(sizes))
    MOI.add_constraint.(model, x, MOI.Interval(1, upper_bound))
    MOI.add_constraint.(model, y, MOI.Interval(1, upper_bound))
    MOI.add_constraint(model, max_x, MOI.Interval(1, upper_bound))
    MOI.add_constraint(model, max_y, MOI.Interval(1, upper_bound))
    MOI.add_constraint.(model, 1max_x .- 1x, MOI.GreaterThan.(sizes))
    MOI.add_constraint.(model, 1max_y .- 1y, MOI.GreaterThan.(sizes))
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x; y; s; s]),
        MiniZinc.MiniZincSet(
            "diffn",
            [1:n, n .+ (1:n), 2n .+ (1:n), 3n .+ (1:n)],
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    obj = (1max_x) * max_y
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) === MOI.FEASIBLE_POINT
    @test MOI.get(model, MOI.ResultCount()) == 1
    @test MOI.get(model, MOI.ObjectiveValue()) == 120
    rm("test_packing.mzn")
    return
end

function test_example_send_more_money()
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
    )
    MOI.set(
        model,
        MOI.RawOptimizerAttribute("model_filename"),
        "test_send_more_money.mzn",
    )
    S, _ = MOI.add_constrained_variable(model, MOI.Interval(1, 9))
    E, _ = MOI.add_constrained_variable(model, MOI.Interval(0, 9))
    N, _ = MOI.add_constrained_variable(model, MOI.Interval(0, 9))
    D, _ = MOI.add_constrained_variable(model, MOI.Interval(0, 9))
    M, _ = MOI.add_constrained_variable(model, MOI.Interval(1, 9))
    O, _ = MOI.add_constrained_variable(model, MOI.Interval(0, 9))
    R, _ = MOI.add_constrained_variable(model, MOI.Interval(0, 9))
    Y, _ = MOI.add_constrained_variable(model, MOI.Interval(0, 9))
    x = [S, E, N, D, M, O, R, Y]
    MOI.add_constraint.(model, x, MOI.Integer())
    f =
        (1_000 * S + 100 * E + 10 * N + D) +
        (1_000 * M + 100 * O + 10 * R + E) -
        (10_000 * M + 1_000 * O + 100 * N + 10 * E + Y)
    MOI.add_constraint.(model, f, MOI.EqualTo(0))
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.AllDifferent(8))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    v = MOI.get(model, MOI.VariablePrimal(), x)
    send = 1_000 * v[1] + 100 * v[2] + 10 * v[3] + v[4]
    more = 1_000 * v[5] + 100 * v[6] + 10 * v[7] + v[2]
    money = 10_000 * v[5] + 1_000 * v[6] + 100 * v[3] + 10 * v[2] + v[8]
    @test send + more == money
    rm("test_send_more_money.mzn")
    return
end

function test_example_sudoku()
    start = [
        5 3 0 0 7 0 0 0 0
        6 0 0 1 9 5 0 0 0
        0 9 8 0 0 0 0 6 0
        8 0 0 0 6 0 0 0 3
        4 0 0 8 0 3 0 0 1
        7 0 0 0 2 0 0 0 6
        0 6 0 0 0 0 2 8 0
        0 0 0 4 1 9 0 0 5
        0 0 0 0 8 0 0 7 9
    ]
    n = 9
    m = 3
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
    )
    MOI.set(
        model,
        MOI.RawOptimizerAttribute("model_filename"),
        "test_sudoku.mzn",
    )
    x = MOI.add_variables(model, n^2)
    X = reshape(x, n, n)
    for i in 1:n, j in 1:n
        sij = start[i, j]
        setij = iszero(sij) ? MOI.Interval(1, n) : MOI.EqualTo(sij)
        MOI.add_constraint(model, X[i, j], setij)
    end
    for i in 1:n, xi in (X[i, :], X[:, i])
        vv = MOI.VectorOfVariables(xi)
        MOI.add_constraint(model, vv, MOI.AllDifferent(n))
    end
    for k in 1:m, l in 1:m
        ii = ((k-1)*m+1):(k*m)
        jj = ((l-1)*m+1):(l*m)
        square = MOI.VectorOfVariables(vec(X[ii, jj]))
        MOI.add_constraint(model, square, MOI.AllDifferent(n))
    end
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    X_sol = reshape(MOI.get(model, MOI.VariablePrimal(), x), n, n)
    sol = [
        5 3 4 6 7 8 9 1 2
        6 7 2 1 9 5 3 4 8
        1 9 8 3 4 2 5 6 7
        8 5 9 7 6 1 4 2 3
        4 2 6 8 5 3 7 9 1
        7 1 3 9 2 4 8 5 6
        9 6 1 5 3 7 2 8 4
        2 8 7 4 1 9 6 3 5
        3 4 5 2 8 6 1 7 9
    ]
    @test X_sol == sol
    f = MOI.ScalarNonlinearFunction(:(!=), Any[X[1, 3], sol[1, 3]])
    MOI.add_constraint(model, f, MOI.EqualTo(1))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.INFEASIBLE
    @test MOI.get(model, MOI.PrimalStatus()) === MOI.NO_SOLUTION
    @test MOI.get(model, MOI.ResultCount()) == 0
    rm("test_sudoku.mzn")
    return
end

end

TestMiniZinc.runtests()
