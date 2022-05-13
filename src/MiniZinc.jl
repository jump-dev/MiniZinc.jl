# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MiniZinc

import Chuffed_jll
import MiniZinc_jll

import MathOptInterface
const MOI = MathOptInterface

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
    ),
    (MOI.BinPacking, MOI.Table),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

include("write.jl")
include("optimize.jl")

end # module
