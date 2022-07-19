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

"""
    Reified(set::MOI.AbstractSet)

The constraint ``[z; f(x)] \\in Reified(S)`` ensures that ``f(x) \\in S`` if and
only if ``z == 1``, where ``z \\in \\{0, 1\\}``.
"""
struct Reified{S<:MOI.AbstractSet} <: MOI.AbstractVectorSet
    set::S
end

MOI.dimension(s::Reified) = 1 + MOI.dimension(s.set)
Base.copy(s::Reified) = Reified(copy(s.set))

const ReifiedLessThan{T} = Reified{MOI.LessThan{T}}
const ReifiedGreaterThan{T} = Reified{MOI.GreaterThan{T}}
const ReifiedEqualTo{T} = Reified{MOI.EqualTo{T}}
const ReifiedBinPacking{T} = Reified{MOI.BinPacking{T}}
const ReifiedTable{T} = Reified{MOI.Table{T}}

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
        # Reified{MOI.Circuit}, Unsupported by MiniZinc
        Reified{MOI.CountAtLeast},
        Reified{MOI.CountBelongs},
        Reified{MOI.CountDistinct},
        Reified{MOI.CountGreaterThan},
        Reified{MOI.Cumulative},
        # Reified{MOI.Path}, Unsupported by MiniZinc
    ),
    (
        MOI.BinPacking,
        MOI.Table,
        ReifiedBinPacking,
        ReifiedTable,
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
    ::Type{<:MOI.AbstractVectorSet},
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
