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
    solver_cmd() do exe
        return String(read(`$exe $args`))
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
struct Optimizer{T} <: MOI.AbstractOptimizer
    solver_cmd::Function
    options::Vector{String}
    inner::Model
    function Optimizer{T}(solver_cmd) where {T}
        return new(solver_cmd, String[], Model{T}())
    end
end

function MOI.supports_constraint(
    model::Optimizer,
    ::Type{F},
    ::Type{S},
) where {F,S}
    return MOI.supports_constraint(model.inner, F, S)
end

function MOI.optimize!(dest::Optimizer, src::MOI.ModelLike)
    MOI.empty!(dest.inner)
    index_map = MOI.copy_to(dest.inner, src)
    return index_map, false
end

# include("model.jl")
# include("export.jl")
# include("optimizer.jl")

end # module
