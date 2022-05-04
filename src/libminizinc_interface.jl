# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

Chuffed() = joinpath(Chuffed_jll.artifact_dir, "chuffed.msc")

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

struct MiniZincExecutable{F}
    f::F
end

function exe_minizinc_jll()
    return MiniZincExecutable(MiniZinc_jll.minizinc)
end

function exe_minizinc_local(
    dir = "/Users/Oscar/Documents/Code/libminizinc/build/install",
)
    return MiniZincExecutable(f -> f(joinpath(dir, "/bin/minizinc")))
end

function run_minizinc(f::MiniZincExecutable, solver::String, filename::String)
    f.f() do exe
        return run(`$(exe) --solver $(solver) $(filename)`)
    end
end
