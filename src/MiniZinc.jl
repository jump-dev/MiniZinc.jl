# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MiniZinc

import Chuffed_jll
import LazyArtifacts
import MiniZinc_jll

function __init__()
    if !(Sys.islinux() || Sys.isapple())
        error("Unsupported platform")
    end
    return
end

import MathOptInterface
const MOI = MathOptInterface

struct Reified{S<:MOI.AbstractSet} <: MOI.AbstractVectorSet
    set::S
end

MOI.dimension(s::Reified) = 1 + MOI.dimension(s.set)
Base.copy(s::Reified) = Reified(copy(s.set))

const ReifiedLessThan{T} = Reified{MOI.LessThan{T}}
const ReifiedGreaterThan{T} = Reified{MOI.GreaterThan{T}}
const ReifiedEqualTo{T} = Reified{MOI.EqualTo{T}}

MOI.Utilities.@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (
        MOI.AllDifferent,
        MOI.Circuit,
        MOI.CountAtLeast,
        MOI.CountBelongs,
        MOI.CountDistinct,
        MOI.CountGreaterThan,
        MOI.Cumulative,
        MOI.Path,
        Reified{MOI.AllDifferent},
        Reified{MOI.CountAtLeast},
        Reified{MOI.CountBelongs},
        Reified{MOI.CountDistinct},
        Reified{MOI.CountGreaterThan},
    ),
    (
        MOI.BinPacking,
        MOI.Table,
        ReifiedLessThan,
        ReifiedGreaterThan,
        ReifiedEqualTo,
    ),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::MOI.AbstractVectorSet,
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{Reified{S}},
) where {T,S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}}}
    return true
end

include("write.jl")
include("optimize.jl")

end # module
