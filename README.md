**Warning: this package is still under active development and requires a non-released
branch of MathOptInterface to function. It is not intended for use (yet).**

# MiniZinc.jl

`MiniZinc.jl` is a wrapper for the [MiniZinc](https://www.minizinc.org)
constraint modeling language.

It provides a way to write MathOptInterface models to `.mzn` files, and a way to
interact with `libminizinc`.

*Note: This wrapper is maintained by the JuMP community and is not part of the
MiniZinc project.*

## Install

Install MiniZinc.jl using the Julia package manager:
```julia
import Pkg
Pkg.add("https://github.com/jump-dev/MiniZinc.jl")
```
You also need a branch of MOI
```julia
Pkg.add(Pkg.PackageSpec(name = "MathOptInterface", rev = "od/cpsat-alldifferent"))
```

**macOS and Windows**

On Linux, this package automatically installs `libminizinc`. However, we're
still working out problems with the install on macOS and Windows. To use
MiniZinc.jl, you'll need to manually install a copy of `libminizinc` from
[minizinc.org](https://www.minizinc.org) or compile one yourself from
[MiniZinc/libminizinc](https://github.com/MiniZinc/libminizinc).

To teach MiniZinc.jl where to look for `libminizinc`, set the
`JULIA_LIBMINIZINC_DIR` environment variable.

For example, I compiled `libminizinc` to:
```julia
ENV["JULIA_LIBMINIZINC_DIR"] = "/Users/Oscar/Documents/Code/libminizinc/build/install"
```

## Use with MathOptInterface

```julia
import MiniZinc
const MOI = MiniZinc.MOI
# If on macOS or Windows, uncomment and change path:
# ENV["JULIA_LIBMINIZINC_DIR"] = "/path/to/libminizinc"
model = MiniZinc.Model{Int}()
x = MOI.add_variables(model, 3)
MOI.add_constraint.(model, x, MOI.Interval(1, 3))
MOI.add_constraint.(model, x, MOI.Integer())
MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.AllDifferent(3))
optimizer = MiniZinc.Optimizer{Int}(MiniZinc.Chuffed())
MOI.optimize!(optimizer, model)
@show MOI.get(optimizer, MOI.VariablePrimal(), x)
```

## Use with JuMP

You can also call MiniZinc from JuMP, using any solver that `libminizinc` supports:
```julia
using JuMP
import MiniZinc
# If on macOS or Windows, uncomment and change path:
# ENV["JULIA_LIBMINIZINC_DIR"] = "/path/to/libminizinc"
model = Model(() -> MiniZinc.Optimizer{Float64}("gurobi"))
@variable(model, 1 <= x[1:3] <= 3, Int)
@constraint(model, x in MOI.AllDifferent(3))
@objective(model, Max, sum(i * x[i] for i in 1:3))
optimize!(model)
@show value.(x)
```
