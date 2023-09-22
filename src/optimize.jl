# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

Chuffed() = joinpath(Chuffed_jll.artifact_dir, "chuffed.msc")

function run_flatzinc(solver_cmd::F, filename, args = String[]) where {F}
    try
        solver_cmd() do exe
            return String(read(`$exe $(vcat(args, filename))`))
        end
    catch
        return ""
    end
end

"""
    Optimizer{T}(solver_cmd) where {T}

Construct a new MiniZinc Optimizer.
"""
mutable struct Optimizer{T} <: MOI.AbstractOptimizer
    solver::String
    inner::Model{T}
    solver_status::String
    primal_objective::T
    primal_solutions::Vector{Dict{MOI.VariableIndex,T}} # all solutions
    options::Dict{String,Any}
    ub_sols::Int # Max number of solutions
    time_limit_sec::Union{Nothing,Float64}
    solve_time_sec::Float64
    function Optimizer{T}(solver::String) where {T}
        if solver == "chuffed"
            solver = Chuffed()
        end
        primal_solutions = Vector{Dict{MOI.VariableIndex,T}}()
        options =
            Dict{String,Any}("model_filename" => "", "all_solutions" => false)
        return new(
            solver,
            Model{T}(),
            "",
            zero(T),
            primal_solutions,
            options,
            1,
            nothing,
            NaN,
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
    elseif Sys.islinux() || Sys.isapple()
        return MiniZinc_jll.minizinc(f)
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

    dest.ub_sols = if dest.options["all_solutions"]
        get(dest.options, "num_solutions", (Int)(typemax(Int32)))
    else
        1 # only one solution if all_solutions is false
    end

    _minizinc_exe() do exe
        cmd = `$(exe) --solver $(dest.solver) --output-objective -o $(output) $(filename)`

        if dest.time_limit_sec !== nothing
            limit = round(Int, 1_000 * dest.time_limit_sec::Float64)
            cmd = `$cmd --time-limit $limit`
        end
        if dest.ub_sols > 1 # only add --num-solutions if ub_sols > 1
            cmd = `$cmd --num-solutions $(dest.ub_sols)`
        end
        return run(pipeline(cmd, stdout = _stdout))
    end
    if isfile(output)
        return read(output, String)
    elseif isfile(_stdout)
        return read(_stdout, String)
    end
    return ""
end

function MOI.get(model::Optimizer, ::MOI.SolverVersion)
    output = sprint() do io
        return _minizinc_exe() do exe
            return run(pipeline(`$(exe) --version`; stdout = io))
        end
    end
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
    model.primal_objective = zero(T)
    empty!(model.primal_solutions)
    model.solve_time_sec = NaN
    return
end

function MOI.supports(model::Optimizer, attr::MOI.RawOptimizerAttribute)
    return haskey(model.options, attr.name)
end

function MOI.get(model::Optimizer, attr::MOI.RawOptimizerAttribute)
    return get(model.options, attr.name, nothing)
end

function MOI.set(model::Optimizer, attr::MOI.RawOptimizerAttribute, value)
    model.options[attr.name] = value
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

function MOI.optimize!(dest::Optimizer{T}, src::MOI.ModelLike) where {T}
    time_start = time()
    MOI.empty!(dest.inner)
    empty!(dest.primal_solutions)
    index_map = MOI.copy_to(dest.inner, src)

    ret = _run_minizinc(dest)
    if !isempty(ret)
        m_stat = match(r"=====(.+)=====", ret)
        if m_stat !== nothing
            @assert length(m_stat.captures) == 1
            dest.solver_status = m_stat[1]
        else
            dest.solver_status = "SATISFIABLE"
            variable_map = Dict(
                MOI.get(dest.inner, MOI.VariableName(), x) => x for
                x in MOI.get(src, MOI.ListOfVariableIndices())
            )

            primal_solution = Dict{MOI.VariableIndex,T}()
            for line in split(ret, "\n")
                m_var = match(r"(.+) \= (.+)\;", line)
                if m_var === nothing
                    isempty(primal_solution) ||
                        push!(dest.primal_solutions, primal_solution) # add solution
                    primal_solution = Dict{MOI.VariableIndex,T}()
                    continue
                elseif m_var[1] == "_objective"
                    dest.primal_objective = _parse_result(T, m_var[2])
                else
                    x = variable_map[m_var[1]]
                    primal_solution[x] = _parse_result(T, m_var[2])
                end
            end
        end
    end
    dest.solve_time_sec = time() - time_start
    return index_map, false
end

function MOI.is_valid(model::Optimizer, x::MOI.VariableIndex)
    return MOI.is_valid(model.inner, x)
end

function _has_solution(model::Optimizer)
    return model.solver_status == "SATISFIABLE" &&
           !isempty(model.primal_solutions)
end

MOI.get(model::Optimizer, ::MOI.RawStatusString) = model.solver_status

function MOI.get(model::Optimizer, ::MOI.TerminationStatus)
    if model.solver_status == "UNSATISFIABLE"
        return MOI.INFEASIBLE
    elseif _has_solution(model)
        if model.ub_sols > 1 && length(model.primal_solutions) >= model.ub_sols
            return MOI.SOLUTION_LIMIT
        else
            return MOI.OPTIMAL
        end
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
    return model.primal_objective
end

function MOI.get(
    model::Optimizer,
    attr::MOI.VariablePrimal,
    x::MOI.VariableIndex,
)
    MOI.check_result_index_bounds(model, attr)
    MOI.throw_if_not_valid(model, x)
    if attr.result_index > length(model.primal_solutions)
        throw(ErrorException("Result index out of bounds."))
    end
    return model.primal_solutions[attr.result_index][x]
end

MOI.get(model::Optimizer, ::MOI.SolveTimeSec) = model.solve_time_sec

function MOI.set(model::Optimizer, attr::MOI.RawOptimizerAttribute, value)
    if attr.name == "all_solutions"
        value === true ||
            value === false ||
            throw(ErrorException("Invalid value $value for $(attr.name)."))
    elseif attr.name == "num_solutions"
        (value isa Int && value >= 1) ||
            throw(ErrorException("Invalid value $value for $(attr.name)."))
    end
    return model.options[attr.name] = value
end
