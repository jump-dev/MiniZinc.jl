# Copyright (c) 2022 FlatZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module FlatZinc

import MathOptInterface
const MOI = MathOptInterface

function run(solver_cmd::F, filename, args = String[]) where {F}
    args = copy(args)
    push!(args, filename)
    try
        solver_cmd() do exe
            return String(read(`$exe $args`))
        end
    catch
        return ""
    end
end

MOI.Utilities.@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (
        MOI.AllDifferent,
        MOI.Among,
        MOI.CountAtLeast,
        MOI.CountDistinct,
        MOI.CountGreaterThan,
    ),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

"""
    Optimizer{T}(solver_cmd) where {T}

Construct a new FlatZinc Optimizer.
"""
mutable struct Optimizer{T} <: MOI.AbstractOptimizer
    solver_cmd::Function
    options::Vector{String}
    inner::Model{T}
    has_solution::Bool
    primal_solution::Dict{MOI.VariableIndex,T}
    function Optimizer{T}(solver_cmd) where {T}
        return new(
            solver_cmd,
            String[],
            Model{T}(),
            false,
            Dict{MOI.VariableIndex,T}(),
        )
    end
end

function MOI.supports_constraint(
    model::Optimizer,
    ::Type{F},
    ::Type{S},
) where {F,S}
    return MOI.supports_constraint(model.inner, F, S)
end

include("write.jl")

function MOI.optimize!(dest::Optimizer{T}, src::MOI.ModelLike) where {T}
    MOI.empty!(dest.inner)
    empty!(dest.primal_solution)
    index_map = MOI.copy_to(dest.inner, src)
    open("temp.fzn", "w") do io
        return write(io, dest.inner)
    end
    variable_map = Dict(
        MOI.get(dest.inner, MOI.VariableName(), x) => x for
        x in MOI.get(src, MOI.ListOfVariableIndices())
    )
    ret = run(dest.solver_cmd, "temp.fzn", dest.options)
    if isempty(ret)
        dest.has_solution = false
    else
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

function MOI.get(
    model::Optimizer,
    attr::MOI.VariablePrimal,
    x::MOI.VariableIndex,
)
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

end # module
