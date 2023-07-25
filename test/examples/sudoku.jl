# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Sudoku example

function test_sudoku()
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
    MOI.set(model, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
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
    # solve
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
    # cut off that solution to make model infeasible
    f = MOI.ScalarNonlinearFunction(:(!=), Any[X[1, 3], sol[1, 3]])
    MOI.add_constraint(model, f, MOI.EqualTo(1))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) === MOI.INFEASIBLE
    @test MOI.get(model, MOI.PrimalStatus()) === MOI.NO_SOLUTION
    @test MOI.get(model, MOI.ResultCount()) == 0
    rm("test.mzn")
    return
end
