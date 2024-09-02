# Inspired from the square packing tutorial in https://www.minizinc.org/
function test_packing()
    # Number of squares
    n = 6
    # Size of each square
    sizes = collect(1:n)
    upper_bound = sum(sizes)

    model = MOI.instantiate(
        () -> MiniZinc.Optimizer{Int}("chuffed");
        with_cache_type = Int,
        with_bridge_type = Int,
    )

    # We need this `s` variable that is trivially equal to `sizes`
    # because `MiniZincSet` only does not take constants
    s = [MOI.add_constrained_variable(model, MOI.Integer())[1] for i in 1:n]
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for i in 1:n]
    y = [MOI.add_constrained_variable(model, MOI.Integer())[1] for i in 1:n]
    max_x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    max_y, _ = MOI.add_constrained_variable(model, MOI.Integer())

    for i in 1:n
        MOI.add_constraint(
            model,
            s[i],
            MOI.EqualTo(sizes[i]),
        )
        MOI.add_constraint(
            model,
            x[i],
            MOI.Interval(1, upper_bound),
        )
        MOI.add_constraint(
            model,
            y[i],
            MOI.Interval(1, upper_bound),
        )
    end
    MOI.add_constraint(
        model,
        max_x,
        MOI.Interval(1, upper_bound),
    )
    MOI.add_constraint(
        model,
        max_y,
        MOI.Interval(1, upper_bound),
    )

    for i in 1:n
        MOI.add_constraint(
            model,
            1max_x - 1x[i],
            MOI.GreaterThan(sizes[i]),
        )
        MOI.add_constraint(
            model,
            1max_y - 1y[i],
            MOI.GreaterThan(sizes[i]),
        )
    end

    MOI.add_constraint(
        model,
        [x; y; s; s],
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
    return
end
