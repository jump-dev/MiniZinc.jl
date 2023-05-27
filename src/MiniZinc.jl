# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MiniZinc

import Chuffed_jll
import MathOptInterface as MOI
import MiniZinc_jll

const ReifiedLessThan{T} = MOI.Reified{MOI.LessThan{T}}
const ReifiedGreaterThan{T} = MOI.Reified{MOI.GreaterThan{T}}
const ReifiedEqualTo{T} = MOI.Reified{MOI.EqualTo{T}}
const ReifiedBinPacking{T} = MOI.Reified{MOI.BinPacking{T}}
const ReifiedTable{T} = MOI.Reified{MOI.Table{T}}

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
        MOI.Reified{MOI.AllDifferent},
        MOI.Reified{MOI.CountAtLeast},
        MOI.Reified{MOI.CountBelongs},
        MOI.Reified{MOI.CountDistinct},
        MOI.Reified{MOI.CountGreaterThan},
        MOI.Reified{MOI.Cumulative},
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
    (MOI.ScalarNonlinearFunction,),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
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

function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{F},
) where {
    T,
    F<:Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
}
    return false
end

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.Reified{S}},
) where {T,S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}}}
    return true
end

_SPECIAL_OPS = Set([:ifelse])

_PREFIX_OPS = Dict(
    :(!) => "not",
    :abs => "abs",
    :exists => "exists",
    :forall => "forall",
    :count => "count",
    :alldifferent => "alldifferent",
    :sum => "sum",
    :maximum => "max",
    :minimum => "min",
    :bool2int => "bool2int",
)

_INFIX_OPS = Dict(
    :|| => "\\/",
    :&& => "/\\",
    :- => "-",
    :+ => "+",
    :* => "*",
    :∧ => "∧",
    :(=) => "=",
    :(==) => "=",
    :(<) => "<",
    :(>) => ">",
    :(<=) => "<=",
    :(>=) => ">=",
    :(=>) => "->",
    :(-->) => "->",
    :(<--) => "<-",
    :⊻ => "xor",
    :(<-->) => "<->",
    :reified => "<->",
    :(!=) => "!=",
)

_SUPPORTED_OPS = reduce(
    vcat,
    collect(d) for d in [_SPECIAL_OPS, keys(_PREFIX_OPS), keys(_INFIX_OPS)]
)

_PREDICATE_NAMES = Set(["alldifferent"])

MOI.get(::Model, ::MOI.ListOfSupportedNonlinearOperators) = _SUPPORTED_OPS

include("write.jl")
include("optimize.jl")

end # module
