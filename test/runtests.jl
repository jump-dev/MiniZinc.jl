# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

import Pkg
Pkg.pkg"add MathOptInterface#master"

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
    constraint int_ge(x1, 1);
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
    include "nvalue.mzn";
    var 1 .. 4: x1;
    var 1 .. 4: x2;
    var 1 .. 4: x3;
    var 1 .. 4: x4;
    constraint nvalue(x1, [x2, x3, x4]);
    solve satisfy;
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
    include "among.mzn";
    var int: x1;
    var int: x2;
    var int: x3;
    var int: x4;
    constraint among(x1, [x2, x3, x4], {3, 4});
    solve satisfy;
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
    include "at_least.mzn";
    var int: x;
    var int: y;
    var int: z;
    constraint at_least(1, [{x, y}, {y, z}], {3});
    solve satisfy;
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
    include "count_gt.mzn";
    var int: c;
    var int: y;
    var int: x1;
    var int: x2;
    var int: x3;
    constraint count_gt([x1, x2, x3], y, c);
    solve satisfy;
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
    include "bin_packing.mzn";
    var int: x1;
    var int: x2;
    constraint bin_packing(2, [x1, x2], [3, 4]);
    solve satisfy;
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
    include "path.mzn";
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
    include "cumulative.mzn";
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
    include "table.mzn";
    var int: x1;
    var int: x2;
    var int: x3;
    constraint table([x1, x2, x3], [| 1, 1, 0 | 0, 1, 1 |]);
    solve satisfy;
    """
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
    include "circuit.mzn";
    var int: x1;
    var int: x2;
    var int: x3;
    constraint circuit([x1, x2, x3]);
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

function test_moi_basic_fzn()
    model = MOI.Utilities.Model{Int}()
    x, x_int = MOI.add_constrained_variable(model, MOI.Integer())
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(1))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(3))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, x_int)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
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
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
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
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    @test MOI.get(solver, MOI.VariablePrimal(), index_map[x]) == 10
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
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
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    v = [MOI.get(solver, MOI.VariablePrimal(), index_map[xi]) for xi in x]
    @test all(v .>= 0)
    @test all(v .<= 1)
    @test sum(v) == 2
    return
end

function test_moi_send_more_money()
    model = MOI.Utilities.Model{Int}()
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
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    v = [MOI.get(solver, MOI.VariablePrimal(), index_map[xi]) for xi in x]
    send = 1_000 * v[1] + 100 * v[2] + 10 * v[3] + v[4]
    more = 1_000 * v[5] + 100 * v[6] + 10 * v[7] + v[2]
    money = 10_000 * v[5] + 1_000 * v[6] + 100 * v[3] + 10 * v[2] + v[8]
    @test send + more == money
    return
end

function test_moi_tests()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Int}(),
        MiniZinc.Optimizer{Int}(MiniZinc.Chuffed()),
    )
    config = MOI.Test.Config(Int)
    MOI.Test.runtests(model, config, include = String["test_cpsat_"])
    return
end

end

TestMiniZinc.runtests()
