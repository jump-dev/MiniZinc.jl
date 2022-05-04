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
    has_solution::Bool
    primal_solution::Dict{MOI.VariableIndex,T}
    function Optimizer{T}(solver::String) where {T}
        return new(solver, Model{T}(), false, Dict{MOI.VariableIndex,T}())
    end
end

exe_minizinc_jll() = MiniZinc_jll.minizinc

function exe_minizinc_local(
    dir = "/Users/Oscar/Documents/Code/libminizinc/build/install",
)
    return f -> f(joinpath(dir, "bin/minizinc"))
end

function _run_minizinc(dest::Optimizer)
    dir = mktempdir()
    filename = joinpath(dir, "model.mzn")
    output = joinpath(dir, "model.ozn")
    open(filename, "w") do io
        return write(io, dest.inner)
    end
    # minizinc = exe_minizinc_local()
    minizinc = exe_minizinc_jll()
    minizinc() do exe
        return run(`$(exe) --solver $(dest.solver) -o $(output) $(filename)`)
    end
    if isfile(output)
        return read(output, String)
    end
    return ""
end

# The MOI interface

function MOI.supports_constraint(
    model::Optimizer,
    ::Type{F},
    ::Type{S},
) where {F,S}
    return MOI.supports_constraint(model.inner, F, S)
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
                dest.primal_solution[x] = tryparse(T, m[2])
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
