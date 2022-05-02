# Copyright (c) 2022 FlatZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module FlatZinc

import MathOptInterface

function run(binary, filename, args = String[])
    args = copy(args)
    push!(args, filename)
    return String(read(`$binary $args`))
end

end # module
