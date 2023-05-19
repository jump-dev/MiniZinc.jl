# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Louvain clustering optimization example

function test_louvain()
    edges = [(1, 2, 1); (1, 3, 4); (1, 5, 7); (2, 4, 10); (3, 5, 12)] # edges and weights
    m = sum(e[3] for e in edges)
    n_nodes = 5
    k = [
        sum((w for (u, v, w) in edges if (u == i || v == i)), init = 0) for
        i in 1:n_nodes
    ]
    n_communities = 2
    # use a caching optimizer for the model
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    model = MOI.Utilities.CachingOptimizer(MOI.Utilities.Model{Int}(), solver)
    # setup model
    x = MOI.add_variables(model, n_nodes)
    MOI.add_constraint(model, x[1], MOI.EqualTo(1))
    MOI.add_constraint.(model, x[2:end], MOI.Interval(1, n_communities))
    terms = Any[]
    for (u, v, w) in edges
        o = 2 * m * w - k[u] * k[v]
        f1 = MOI.ScalarNonlinearFunction(:(=), Any[x[u], x[v]])
        f2 = MOI.ScalarNonlinearFunction(:ifelse, Any[f1, o, 0])
        push!(terms, f2)
    end
    f = MOI.ScalarNonlinearFunction(:+, terms)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    # solve
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(model, MOI.ResultCount()) >= 1
    x_sol = MOI.get(model, MOI.VariablePrimal(), x)
    @test x_sol == [1, 2, 1, 2, 1]
    # MOI.get(model, MOI.ObjectiveValue()) # TODO test in future when MOI allows evaluating SNF value
    rm("test.mzn")
    return
end
