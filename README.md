# MiniZinc.jl

[MiniZinc.jl](https://github.com/jump-dev/MiniZinc.jl) is a wrapper for the
[MiniZinc](https://www.minizinc.org) constraint modeling language.

It provides a way to write MathOptInterface models to `.mzn` files, and a way to
interact with `libminizinc`.

## Affiliation

This wrapper is maintained by the JuMP community and is not part of the MiniZinc
project.

## Getting help

If you need help, please ask a question on the [JuMP community forum](https://jump.dev/forum).

If you have a reproducible example of a bug, please [open a GitHub issue](https://github.com/jump-dev/MiniZinc.jl/issues/new).

## License

`MiniZinc.jl` is licensed under the [MIT License](https://github.com/jump-dev/MiniZinc.jl/blob/master/LICENSE.md).

The underlying project, [MiniZinc/libminizinc](https://github.com/MiniZinc/libminizinc),
is licensed under the [MPL 2.0 license](https://github.com/MiniZinc/libminizinc/blob/master/LICENSE.txt).

## Install

Install MiniZinc.jl using the Julia package manager:
```julia
import Pkg
Pkg.add("MiniZinc")
```

To use a custom install of MiniZinc, set the `JULIA_LIBMINIZINC_DIR` environment
variable. For example:
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

julia> import MathOptInterface as MOI

julia> function main()
           model = MOI.Utilities.CachingOptimizer(
               MiniZinc.Model{Int}(),
               MiniZinc.Optimizer{Int}("chuffed"),
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
supports. By default, MiniZinc.jl is compiled with the
[HiGHS](https://github.com/ERGO-Code/HiGHS) MILP solver,
which can be selected by passing the `"highs"` parameter to `MiniZinc.Optimizer`:

```julia
using JuMP
import MiniZinc
model = Model(() -> MiniZinc.Optimizer{Float64}("highs"))
@variable(model, 1 <= x[1:3] <= 3, Int)
@constraint(model, x in MOI.AllDifferent(3))
@objective(model, Max, sum(i * x[i] for i in 1:3))
optimize!(model)
@show value.(x)
```

## MathOptInterface API

The MiniZinc `Optimizer{T}` supports the following constraints and attributes.

List of supported objective functions:

 * [`MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}`](@ref)
 * [`MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}`](@ref)
 * [`MOI.ObjectiveFunction{MOI.VariableIndex}`](@ref)

List of supported variable types:

 * [`MOI.Reals`](@ref)

List of supported constraint types:

 * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
 * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.GreaterThan{T}`](@ref)
 * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.Integer`](@ref)
 * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.Interval{T}`](@ref)
 * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
 * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.ZeroOne`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.EqualTo{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.GreaterThan{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.Integer`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.Interval{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.LessThan{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.Parameter{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.Semicontinuous{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.Semiinteger{T}`](@ref)
 * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.AllDifferent`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.BinPacking{T}`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.Circuit`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.CountAtLeast`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.CountBelongs`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.CountDistinct`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.CountGreaterThan`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.Cumulative`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.Path`](@ref)
 * [`MOI.VectorOfVariables`](@ref) in [`MOI.Table{T}`](@ref)

List of supported model attributes:

 * [`MOI.NLPBlock()`](@ref)
 * [`MOI.Name()`](@ref)
 * [`MOI.ObjectiveSense()`](@ref)

## Options

Set options using [`MOI.RawOptimizerAttribute`](@ref) in MOI or
[`set_attribute`](@ref) in JuMP.

`MiniZinc.jl` supports the following options:

 * `model_filename::String = ""`: the location at which to write out the `.mzn`
   file during optimization. This option can be helpful during debugging. If
   left empty, a temporary file will be used instead.

 * [`MOI.SolutionLimit`](@ref): set this option to a positive integer to return
   up to the `limit` number of solutions.
