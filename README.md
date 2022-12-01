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
Pkg.add("MiniZinc")
```

**Windows**

On Linux and macOS, this package automatically installs `libminizinc`. However,
we're still working out problems with the install on Windows. To use
MiniZinc.jl, you'll need to manually install a copy of `libminizinc` from
[minizinc.org](https://www.minizinc.org) or compile one yourself from
[MiniZinc/libminizinc](https://github.com/MiniZinc/libminizinc).

To teach MiniZinc.jl where to look for `libminizinc`, set the
`JULIA_LIBMINIZINC_DIR` environment variable. For example:
```julia
ENV["JULIA_LIBMINIZINC_DIR"] = "C:\\Program Files\\MiniZinc"
```

## Use with MathOptInterface

MiniZinc.jl supports the [constraint programming sets](https://jump.dev/MathOptInterface.jl/stable/reference/standard_form/#Constraint-programming-sets)
defined in MathOptInterface, as well as (in)equality constraints.

The following example solves the following constraint program:
```
xᵢ ∈ {1, 2, 3} ∀i=1,2,3
zⱼ ∈ {0, 1}    ∀j=1,2
z₁ <-> x₁ != x₂
z₂ <-> x₂ != x₃
z₁ + z₂ = 1
```

```julia
julia> import MiniZinc

julia> const MOI = MiniZinc.MOI
MathOptInterface

julia> function main()
           model = MOI.Utilities.CachingOptimizer(
               MiniZinc.Model{Int}(),
               MiniZinc.Optimizer{Int}(MiniZinc.Chuffed()),
           )
           # xᵢ ∈ {1, 2, 3} ∀i=1,2,3
           x = MOI.add_variables(model, 3)
           MOI.add_constraint.(model, x, MOI.Interval(1, 3))
           MOI.add_constraint.(model, x, MOI.Integer())
           # zⱼ ∈ {0, 1}    ∀j=1,2
           z = MOI.add_variables(model, 2)
           MOI.add_constraint.(model, z, MOI.ZeroOne())
           # z₁ <-> x₁ != x₂
           MOI.add_constraint(
               model,
               MOI.VectorOfVariables([z[1], x[1], x[2]]),
               MOI.Reified(MOI.AllDifferent(2)),
           )
           # z₂ <-> x₂ != x₃
           MOI.add_constraint(
               model,
               MOI.VectorOfVariables([z[2], x[2], x[3]]),
               MOI.Reified(MOI.AllDifferent(2)),
           )
           # z₁ + z₂ = 1
           MOI.add_constraint(model, 1 * z[1] + x[2], MOI.EqualTo(1))
           MOI.optimize!(model)
           x_star = MOI.get(model, MOI.VariablePrimal(), x)
           z_star = MOI.get(model, MOI.VariablePrimal(), z)
           return x_star, z_star
       end
main (generic function with 1 method)

julia> main()
([1, 1, 3], [0, 1])
```

## Use with JuMP

You can also call MiniZinc from JuMP, using any solver that `libminizinc`
supports. For example, if you have Gurobi installed:
```julia
using JuMP
import MiniZinc
model = Model(() -> MiniZinc.Optimizer{Float64}("gurobi"))
@variable(model, 1 <= x[1:3] <= 3, Int)
@constraint(model, x in MOI.AllDifferent(3))
@objective(model, Max, sum(i * x[i] for i in 1:3))
optimize!(model)
@show value.(x)
```
