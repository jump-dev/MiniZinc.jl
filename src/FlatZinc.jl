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

"""
    Optimizer{T}(solver_cmd) where {T}

Construct a new FlatZinc Optimizer.
"""
struct Optimizer{T}
    solver_cmd::Function
    options::Vector{String}
    function Optimizer{T}(solver_cmd) where {T}
        return new(solver_cmd, String[])
    end
end

# include("model.jl")
# include("export.jl")
# include("optimizer.jl")

end # module
