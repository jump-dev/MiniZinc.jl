# Copyright (c) 2022 FlatZinc.jl contributors
# Copyright (c) 2020 Thibaut Cuvelier
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function _variable_info(model::Model{T}, x) where {T}
    name = MOI.get(model, MOI.VariableName(), x)
    F = MOI.VariableIndex
    lb, ub = typemin(T), typemax(T)
    # ZeroOne
    ci_zero = MOI.ConstraintIndex{F,MOI.ZeroOne}(x.value)
    is_bin = MOI.is_valid(model, ci_zero)
    # Integer
    ci_int = MOI.ConstraintIndex{F,MOI.Integer}(x.value)
    is_int = MOI.is_valid(model, ci_int)
    # GreaterThan
    ci_lb = MOI.ConstraintIndex{F,MOI.GreaterThan{T}}(x.value)
    if MOI.is_valid(model, ci_lb)
        lb = MOI.get(model, MOI.ConstraintSet(), ci_lb).lower
    end
    # LessThan
    ci_ub = MOI.ConstraintIndex{F,MOI.LessThan{T}}(x.value)
    if MOI.is_valid(model, ci_ub)
        ub = MOI.get(model, MOI.ConstraintSet(), ci_ub).upper
    end
    # EqualTo
    ci_eq = MOI.ConstraintIndex{F,MOI.EqualTo{T}}(x.value)
    if MOI.is_valid(model, ci_eq)
        lb = ub = MOI.get(model, MOI.ConstraintSet(), ci_eq).value
    end
    # Interval
    ci_iv = MOI.ConstraintIndex{F,MOI.Interval{T}}(x.value)
    if MOI.is_valid(model, ci_iv)
        set = MOI.get(model, MOI.ConstraintSet(), ci_iv)
        lb, ub = set.lower, set.upper
    end
    if is_bin || is_int
        lb, ub = ceil(Int, lb), floor(Int, ub)
    end
    return (name = name, lb = lb, ub = ub, is_int = is_int, is_bin = is_bin)
end

function _write_variables(io::IO, model::Model{T}) where {T}
    variables = Dict(
        x => _variable_info(model, x) for
        x in MOI.get(model, MOI.ListOfVariableIndices())
    )
    constraint_lines = ""
    for (x, info) in variables
        lb, ub = info.lb, info.ub
        if info.is_bin
            if lb == ub == 0
                print(io, "var bool: $(info.name) :: output_var = false;")
            elseif lb == ub == 1
                print(io, "var bool: $(info.name) :: output_var = true;")
            else
                print(io, "var bool: $(info.name) :: output_var;")
            end
        elseif info.is_int
            if lb == ub
                print(io, "var int: $(info.name) :: output_var = $lb;")
            elseif typemin(T) < lb && ub < typemax(T)
                print(io, "var $lb .. $ub: $(info.name) :: output_var;")
            else
                print(io, "var int: $(info.name) :: output_var;")
                if ub < typemax(T)
                    constraint_lines *= "constraint int_lt($(info.name), $ub);\n"
                end
                if typemin(T) < lb
                    constraint_lines *= "constraint int_lt($lb, $(info.name));\n"
                end
            end
        else
            if lb == ub
                print(io, "var float: $(info.name) :: output_var = $lb;")
            elseif typemin(T) < lb && ub < typemax(T)
                print(io, "var $lb .. $ub: $(info.name) :: output_var;")
            else
                print(io, "var float: $(info.name) :: output_var;")
                if ub < typemax(T)
                    constraint_lines *= "constraint float_lt($(info.name), $ub);\n"
                end
                if typemin(T) < lb
                    constraint_lines *= "constraint float_lt($lb, $(info.name));\n"
                end
            end
        end
    end
    return variables, constraint_lines
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{MOI.AllDifferent},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        print(io, "constraint all_different_int")
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        prefix = "(["
        for x in f.variables
            print(io, prefix, variables[x].name)
            prefix = ", "
        end
        println(io, "]);")
    end
    return
end

_set_to_constraint(::Type{<:MOI.LessThan{<:Integer}}) = "int_lin_le"
_set_to_constraint(::Type{<:MOI.GreaterThan{<:Integer}}) = "int_lin_ge"
_set_to_constraint(::Type{<:MOI.EqualTo{<:Integer}}) = "int_lin_eq"

_set_to_constraint(::Type{<:MOI.LessThan{<:Real}}) = "float_lin_le"
_set_to_constraint(::Type{<:MOI.GreaterThan{<:Real}}) = "float_lin_ge"
_set_to_constraint(::Type{<:MOI.EqualTo{<:Real}}) = "float_lin_eq"

_rhs(s::MOI.LessThan) = s.upper
_rhs(s::MOI.GreaterThan) = s.lower
_rhs(s::MOI.EqualTo) = s.value

function _write_constraint(
    io::IO,
    model::Model{T},
    variables,
    F::Type{MOI.ScalarAffineFunction{T}},
    S::Type{<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}}},
) where {T}
    set = _set_to_constraint(S)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        coefs = join(["$(t.coefficient)" for t in f.terms], ", ")
        vars = join([_to_string(variables, t.variable) for t in f.terms], ", ")
        rhs = "$(_rhs(s) - f.constant)"
        println(io, "constraint $set ([$coefs], [$vars], $rhs);")
    end
    return
end

_to_string(variables, f::MOI.VariableIndex) = variables[f].name

function Base.write(io::IO, model::Model{T}) where {T}
    MOI.FileFormats.create_unique_variable_names(
        model,
        false,
        [
            s -> match(r"^[^a-zA-Z]", s) !== nothing ? "x" * s : s,
            s -> replace(s, r"[^A-Za-z0-9_]" => "_"),
        ],
    )
    variables, constraint_lines = _write_variables(io, model)
    println(io, constraint_lines)
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if F == MOI.VariableIndex
            continue
        end
        _write_constraint(io, model, variables, F, S)
    end
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        println(io, "solve satisfy;")
    else
        print(io, "solve ")
        print(io, sense == MOI.MAX_SENSE ? "maximize " : "minimize ")
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        f = MOI.get(model, MOI.ObjectiveFunction{F}())
        print(io, _to_string(variables, f))
        println(io, ";")
    end
    return
end
