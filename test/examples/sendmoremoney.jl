# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# send more money example
# based on MiniZinc example send-more-money.mzn

function test_send_more_money()
    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
    )
    MOI.set(model, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
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
    # solve
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    v = MOI.get(model, MOI.VariablePrimal(), x)
    send = 1_000 * v[1] + 100 * v[2] + 10 * v[3] + v[4]
    more = 1_000 * v[5] + 100 * v[6] + 10 * v[7] + v[2]
    money = 10_000 * v[5] + 1_000 * v[6] + 100 * v[3] + 10 * v[2] + v[8]
    @test send + more == money
    rm("test.mzn")
    return
end
