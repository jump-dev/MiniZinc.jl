# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function Chuffed()
    for subdir in ["", joinpath("share", "minizinc", "solvers")]
        file = joinpath(Chuffed_jll.artifact_dir, subdir, "chuffed.msc")
        if isfile(file)
            return file
        end
    end
end

function run_flatzinc(solver_cmd::F, filename, args = String[]) where {F}
    return read(`$(solver_cmd()) $(vcat(args, filename))`, String)
end

"""
    Optimizer{T}(solver_cmd) where {T}

Construct a new MiniZinc Optimizer.
"""
mutable struct Optimizer{T} <: MOI.AbstractOptimizer
    solver::String
    inner::Model{T}
    solver_status::String
    primal_objectives::Vector{T}
    primal_solutions::Vector{Dict{MOI.VariableIndex,T}}
    options::Dict{String,Any}
    silent::Bool
    time_limit_sec::Union{Nothing,Float64}
    solve_time_sec::Float64
    # Conflict (IIS) state, populated by `compute_conflict!`. `conflict_status`
    # tracks whether a conflict computation ran and its outcome;
    # `conflict_constraints` holds the inner `ConstraintIndex`es that findMUS
    # reported as part of a minimal unsatisfiable subset.
    conflict_status::MOI.ConflictStatusCode
    conflict_constraints::Set{MOI.ConstraintIndex}
    function Optimizer{T}(solver::String) where {T}
        if solver == "chuffed"
            solver = Chuffed()
        end
        primal_solutions = Dict{MOI.VariableIndex,T}[]
        options = Dict{String,Any}(
            "model_filename" => "",
            "num_solutions" => nothing,
            "all_solutions" => false,
        )
        return new(
            solver,
            Model{T}(),
            "",
            T[],
            primal_solutions,
            options,
            false,
            nothing,
            NaN,
            MOI.COMPUTE_CONFLICT_NOT_CALLED,
            Set{MOI.ConstraintIndex}(),
        )
    end
end

function _minizinc_exe(f::F) where {F}
    user_dir = get(ENV, "JULIA_LIBMINIZINC_DIR", nothing)
    if user_dir !== nothing
        if isfile(joinpath(user_dir, "bin/minizinc"))
            return f(joinpath(user_dir, "bin/minizinc"))
        else
            return f(joinpath(user_dir, "minizinc"))
        end
    elseif MiniZinc_jll.is_available()
        return f(MiniZinc_jll.minizinc())
    end
    return error(
        "Unable to call libminizinc. Please manually install a copy and set " *
        "the `JULIA_LIBMINIZINC_DIR` environment variable. See the README.md " *
        "for more details",
    )
end

function _run_minizinc(dest::Optimizer)
    dir = mktempdir()
    filename = dest.options["model_filename"]
    if isempty(filename)
        filename = joinpath(dir, "model.mzn")
    end
    open(filename, "w") do io
        return write(io, dest.inner)
    end
    output = joinpath(dir, "model.ozn")
    _stdout = joinpath(dir, "_stdout.txt")
    _stderr = joinpath(dir, "_stderr.txt")
    try
        _minizinc_exe() do exe
            cmd = `$(exe) --solver $(dest.solver) --output-objective -o $(output) $(filename)`
            if dest.time_limit_sec !== nothing
                limit = round(Int, 1_000 * dest.time_limit_sec::Float64)
                cmd = `$cmd --time-limit $limit`
            end
            if dest.options["num_solutions"] !== nothing
                cmd = `$cmd --num-solutions $(dest.options["num_solutions"])`
            end
            if dest.options["all_solutions"]
                cmd = `$cmd -a`
            end
            if !dest.silent
                cmd = `$cmd --verbose-solving`
            end
            # use `open(...)` blocks so that it always flushes, even on errors
            open(_stdout, "w") do stdout
                open(_stderr, "w") do err_io
                    stderr = dest.silent ? err_io : _Tee(err_io, Base.stderr)
                    run(pipeline(cmd; stdout, stderr))
                    return
                end
            end
        end
    catch
        status = "=====ERROR=====\n"
        if isfile(_stdout)
            status *= read(_stdout, String)
        end
        if isfile(_stderr)
            status *= read(_stderr, String)
        end
        return status
    end
    if isfile(output)
        return read(output, String)
    elseif isfile(_stdout)
        return read(_stdout, String)
    end
    return ""
end

function MOI.get(model::Optimizer, ::MOI.SolverVersion)
    output = _minizinc_exe(exe -> read(`$(exe) --version`, String))
    @assert !isempty(output)
    m = match(r"version (\d+.\d+.\d+)", output)::RegexMatch
    return VersionNumber(m[1])
end

# The MOI interface

MOI.get(model::Optimizer, ::MOI.SolverName) = "MiniZinc"

MOI.is_empty(model::Optimizer) = MOI.is_empty(model.inner)

function MOI.empty!(model::Optimizer{T}) where {T}
    MOI.empty!(model.inner)
    empty!(model.inner.ext)
    model.solver_status = ""
    empty!(model.primal_objectives)
    empty!(model.primal_solutions)
    model.solve_time_sec = NaN
    model.conflict_status = MOI.COMPUTE_CONFLICT_NOT_CALLED
    empty!(model.conflict_constraints)
    return
end

function MOI.supports(model::Optimizer, attr::MOI.RawOptimizerAttribute)
    return haskey(model.options, attr.name)
end

function MOI.get(model::Optimizer, attr::MOI.RawOptimizerAttribute)
    return get(model.options, attr.name, nothing)
end

function MOI.set(model::Optimizer, attr::MOI.RawOptimizerAttribute, value)
    if attr.name == "num_solutions"
        MOI.set(model, MOI.SolutionLimit(), value)
        return
    elseif attr.name == "all_solutions"
        MOI.set(model, AllSolutions(), value)
        return
    end
    model.options[attr.name] = value
    return
end

MOI.supports(::Optimizer, ::MOI.SolutionLimit) = true

function MOI.get(model::Optimizer, ::MOI.SolutionLimit)
    return MOI.get(model, MOI.RawOptimizerAttribute("num_solutions"))
end

function MOI.set(
    model::Optimizer,
    attr::MOI.SolutionLimit,
    value::Union{Nothing,Integer},
)
    if value isa Integer && value < 1
        msg = "value must be an `Int` that is >= 1"
        throw(MOI.SetAttributeNotAllowed(attr, msg))
    end
    model.options["num_solutions"] = value
    return
end

"""
    AllSolutions() <: MOI.AbstractOptimizerAttribute

An optimizer attribute to report all solutions found by the solver. Maps to
the `-a` flag in MiniZinc. This is `false` by default.

When `true`, satisfaction problems report and solve for every solution (up to
[`MOI.SolutionLimit`](@ref)) and optimization problems report every improving
solution.
"""
struct AllSolutions <: MOI.AbstractOptimizerAttribute end

MOI.supports(::Optimizer, ::AllSolutions) = true

function MOI.get(model::Optimizer, ::AllSolutions)
    return MOI.get(model, MOI.RawOptimizerAttribute("all_solutions"))
end

function MOI.set(model::Optimizer, ::AllSolutions, value::Bool)
    model.options["all_solutions"] = value
    return
end

MOI.supports(::Optimizer, ::MOI.Silent) = true

MOI.get(model::Optimizer, ::MOI.Silent) = model.silent

function MOI.set(model::Optimizer, ::MOI.Silent, value::Bool)
    model.silent = value
    return
end

MOI.supports(::Optimizer, ::MOI.TimeLimitSec) = true

MOI.get(model::Optimizer, ::MOI.TimeLimitSec) = model.time_limit_sec

function MOI.set(model::Optimizer, ::MOI.TimeLimitSec, value::Real)
    model.time_limit_sec = convert(Float64, value)
    return
end

function MOI.set(model::Optimizer, ::MOI.TimeLimitSec, ::Nothing)
    model.time_limit_sec = nothing
    return
end

function MOI.supports_constraint(
    model::Optimizer,
    ::Type{F},
    ::Type{S},
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    return MOI.supports_constraint(model.inner, F, S)
end

function MOI.supports(model::Optimizer, attr::MOI.AbstractModelAttribute)
    return MOI.supports(model.inner, attr)
end

function _parse_result(::Type{T}, s::AbstractString) where {T}
    if s == "true"
        return T(1)
    elseif s == "false"
        return T(0)
    else
        return tryparse(T, s)
    end
end

function _parse_status(ret::String)
    # MiniZinc status are written like `=====STATUS=====` or `==========` 
    # see https://docs.minizinc.org/en/stable/fzn-spec.html#output
    m_stat = match(r"^=====([A-Za-z]*)=====\r?$"m, ret)
    if m_stat === nothing
        # has solutions, but stopped early
        return "SATISFIABLE"
    elseif isempty(m_stat[1])
        # completed the search (ie optimal)
        return "OPTIMAL"
    elseif m_stat[1] == "ERROR"
        # keep the full log for the user
        return ret
    else
        # UNSATISFIABLE, UNBOUNDED, UNSATorUNBOUNDED or UNKNOWN.
        return m_stat[1]
    end
end

function _parse_solutions!(
    dest::Optimizer{T},
    src::MOI.ModelLike,
    ret::String,
) where {T}
    variable_map = Dict(
        MOI.get(dest.inner, MOI.VariableName(), x) => x for
        x in MOI.get(src, MOI.ListOfVariableIndices())
    )
    primal_solution = Dict{MOI.VariableIndex,T}()
    objective = zero(T)
    for line in split(ret, "\n")
        m_var = match(r"(.+) \= (.+)\;", line)
        if m_var === nothing
            if !isempty(primal_solution)
                # We found a line in the output that is not a variable
                # statement. It must divide the solutions, so append
                # the current.
                push!(dest.primal_solutions, copy(primal_solution))
                push!(dest.primal_objectives, objective)
                empty!(primal_solution)
            end
        elseif m_var[1] == "_objective"
            objective = _parse_result(T, m_var[2])
        else
            x = variable_map[m_var[1]]
            primal_solution[x] = _parse_result(T, m_var[2])
        end
    end
    if MOI.get(dest.inner, MOI.ObjectiveSense()) != MOI.FEASIBILITY_SENSE
        # `-i` prints the solutions in order of improvement, but MOI requires
        # the best solution to be first
        reverse!(dest.primal_solutions)
        reverse!(dest.primal_objectives)
    end
    return
end

function MOI.optimize!(dest::Optimizer{T}, src::MOI.ModelLike) where {T}
    time_start = time()
    MOI.empty!(dest.inner)
    empty!(dest.primal_objectives)
    empty!(dest.primal_solutions)
    # A re-solve invalidates any conflict computed for the previous model.
    dest.conflict_status = MOI.COMPUTE_CONFLICT_NOT_CALLED
    empty!(dest.conflict_constraints)
    index_map = MOI.copy_to(dest.inner, src)
    ret = _run_minizinc(dest)
    if !isempty(ret)
        dest.solver_status = _parse_status(ret)
        if dest.solver_status in ("SATISFIABLE", "OPTIMAL")
            _parse_solutions!(dest, src, ret)
        end
    end
    dest.solve_time_sec = time() - time_start
    return index_map, false
end

function MOI.is_valid(model::Optimizer, x::MOI.VariableIndex)
    return MOI.is_valid(model.inner, x)
end

function _has_solution(model::Optimizer)
    return model.solver_status in ("SATISFIABLE", "OPTIMAL") &&
           !isempty(model.primal_solutions)
end

MOI.get(model::Optimizer, ::MOI.RawStatusString) = model.solver_status

function MOI.get(model::Optimizer, ::MOI.TerminationStatus)
    if _has_solution(model)
        num_solutions = something(model.options["num_solutions"], 0)
        sense = MOI.get(model.inner, MOI.ObjectiveSense())
        if model.solver_status == "OPTIMAL"
            # completed the search
            return MOI.OPTIMAL
        elseif (
            sense == MOI.FEASIBILITY_SENSE &&
            num_solutions == 0 &&
            !model.options["all_solutions"]
        )
            # no solution limit, so stopped after the first solution
            return MOI.OPTIMAL
        elseif (
            sense == MOI.FEASIBILITY_SENSE &&
            0 < num_solutions <= length(model.primal_solutions)
        )
            # solution limits are only enforced for satisfaction problems
            return MOI.SOLUTION_LIMIT
        elseif model.time_limit_sec !== nothing
            # did not complete search, and a time limit does exist,
            # so it must have timed out
            return MOI.TIME_LIMIT
        else
            return MOI.OTHER_LIMIT
        end
    elseif model.solver_status == "UNSATISFIABLE"
        return MOI.INFEASIBLE
    elseif model.solver_status == "UNBOUNDED"
        return MOI.DUAL_INFEASIBLE
    elseif model.solver_status == "UNSATorUNBOUNDED"
        return MOI.INFEASIBLE_OR_UNBOUNDED
    elseif (
        model.solver_status == "UNKNOWN" &&
        model.time_limit_sec !== nothing &&
        model.solve_time_sec >= model.time_limit_sec
    )
        return MOI.TIME_LIMIT
    else
        return MOI.OTHER_ERROR
    end
end

function MOI.get(model::Optimizer, ::MOI.PrimalStatus)
    if _has_solution(model)
        return MOI.FEASIBLE_POINT
    else
        return MOI.NO_SOLUTION
    end
end

MOI.get(::Optimizer, ::MOI.DualStatus) = MOI.NO_SOLUTION

MOI.get(model::Optimizer, ::MOI.ResultCount) = length(model.primal_solutions)

function MOI.get(model::Optimizer, attr::MOI.ObjectiveValue)
    MOI.check_result_index_bounds(model, attr)
    return model.primal_objectives[attr.result_index]
end

function MOI.get(
    model::Optimizer,
    attr::MOI.VariablePrimal,
    x::MOI.VariableIndex,
)
    MOI.check_result_index_bounds(model, attr)
    MOI.throw_if_not_valid(model, x)
    return model.primal_solutions[attr.result_index][x]
end

MOI.get(model::Optimizer, ::MOI.SolveTimeSec) = model.solve_time_sec
