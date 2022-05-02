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

_to_string(model, variables, f::MOI.VariableIndex) = variables[f].name

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
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        println(io, "solve satisfy;")
    else
        print(io, "solve ")
        print(io, sense == MOI.MAX_SENSE ? "maximize " : "minimize ")
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        f = MOI.get(model, MOI.ObjectiveFunction{F}())
        print(io, _to_string(model, variables, f))
        println(io, ";")
    end
    return
end
