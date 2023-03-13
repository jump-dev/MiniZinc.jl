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
    model = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(model, n^2)
    X = reshape(x, n, n)
    for i in 1:n, j in 1:n
        sij = start[i, j]
        xij = X[i, j]
        if iszero(sij)
            MOI.add_constraint.(model, xij, MOI.Interval(1, n))
        else
            MOI.add_constraint(model, xij, MOI.EqualTo(sij))
        end
    end
    for i in 1:n
        row = MOI.VectorOfVariables(X[i, :])
        col = MOI.VectorOfVariables(X[:, i])
        MOI.add_constraint(model, row, MOI.AllDifferent(n))
        MOI.add_constraint(model, col, MOI.AllDifferent(n))
    end
    for k in 1:m, l in 1:m
        ii = ((k-1)*m+1):(k*m)
        jj = ((l-1)*m+1):(l*m)
        square = MOI.VectorOfVariables(vec(X[ii, jj]))
        MOI.add_constraint(model, square, MOI.AllDifferent(n))
    end
    # solve
    solver = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
    MOI.set(solver, MOI.RawOptimizerAttribute("model_filename"), "test.mzn")
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.OPTIMAL
    @test MOI.get(solver, MOI.ResultCount()) >= 1
    x_sol = MOI.get(solver, MOI.VariablePrimal(), [index_map[v] for v in x])
    X_sol = reshape(x_sol, n, n)
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
    MOI.empty!(solver)
    @test MOI.is_empty(solver)
    f = MOI.ScalarNonlinearFunction(:(!=), Any[X[1, 3], sol[1, 3]])
    MOI.add_constraint(model, f, MOI.EqualTo(1))
    index_map, _ = MOI.optimize!(solver, model)
    @test MOI.get(solver, MOI.TerminationStatus()) === MOI.INFEASIBLE
    @test MOI.get(solver, MOI.PrimalStatus()) === MOI.NO_SOLUTION
    @test MOI.get(solver, MOI.ResultCount()) == 0
    rm("test.mzn")
    return
end
