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

function _artifact_path()
    return joinpath(
        LazyArtifacts.artifact"minizinc",
        "MiniZinc.x86_64-apple-darwin",
        "bin",
        "minizinc",
    )
end

"""
    Optimizer{T}(solver_cmd) where {T}

Construct a new MiniZinc Optimizer.
"""
mutable struct Optimizer{T} <: MOI.AbstractOptimizer
    solver::String
    inner::Model{T}
    has_solution::Bool
    primal_solution::Dict{MOI.VariableIndex,T}
    options::Dict{String,Any}
    function Optimizer{T}(solver::String) where {T}
        primal_solution = Dict{MOI.VariableIndex,T}()
        options = Dict{String,Any}("model_filename" => "")
        return new(solver, Model{T}(), false, primal_solution, options)
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
    elseif Sys.islinux()
        return MiniZinc_jll.minizinc(f)
    elseif Sys.isapple()
        return f(_artifact_path())
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
    output = joinpath(dir, "model.ozn")
    open(filename, "w") do io
        return write(io, dest.inner)
    end
    _minizinc_exe() do exe
        return run(`$(exe) --solver $(dest.solver) -o $(output) $(filename)`)
    end
    if isfile(output)
        return read(output, String)
    end
    return ""
end

# The MOI interface

MOI.is_empty(model::Optimizer) = MOI.is_empty(model.inner)

function MOI.empty!(model::Optimizer)
    MOI.empty!(model.inner)
    model.has_solution = false
    empty!(model.primal_solution)
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
    MOI.empty!(dest.inner)
    empty!(dest.primal_solution)
    index_map = MOI.copy_to(dest.inner, src)
    ret = _run_minizinc(dest)
    if isempty(ret)
        dest.has_solution = false
    else
        variable_map = Dict(
            MOI.get(dest.inner, MOI.VariableName(), x) => x for
            x in MOI.get(src, MOI.ListOfVariableIndices())
        )
        dest.has_solution = true
        for line in split(ret, "\n")
            m = match(r"(.+) \= (.+)\;", line)
            if m !== nothing
                x = variable_map[m[1]]
                dest.primal_solution[x] = _parse_result(T, m[2])
            end
        end
    end
    return index_map, false
end

function MOI.get(model::Optimizer, ::MOI.VariablePrimal, x::MOI.VariableIndex)
    return model.primal_solution[x]
end

function MOI.get(model::Optimizer, ::MOI.TerminationStatus)
    if model.has_solution
        return MOI.OPTIMAL
    else
        return MOI.OTHER_ERROR
    end
end

function MOI.get(model::Optimizer, ::MOI.PrimalStatus)
    if model.has_solution
        return MOI.FEASIBLE_POINT
    else
        return MOI.NO_SOLUTION
    end
end

MOI.get(::Optimizer, ::MOI.DualStatus) = MOI.NO_SOLUTION

function MOI.get(model::Optimizer, ::MOI.ResultCount)
    return model.has_solution ? 1 : 0
end
