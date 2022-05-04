# Copyright (c) 2022 MiniZinc.jl contributors
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
    all_variables = MOI.get(model, MOI.ListOfVariableIndices())
    variables = Dict(x => _variable_info(model, x) for x in all_variables)
    constraint_lines = ""
    for x in all_variables
        info = variables[x]
        lb, ub = info.lb, info.ub
        if info.is_bin
            if lb == ub == 0
                print(io, "var bool: $(info.name) = false;")
            elseif lb == ub == 1
                print(io, "var bool: $(info.name) = true;")
            else
                print(io, "var bool: $(info.name);")
            end
        elseif info.is_int
            if lb == ub
                print(io, "var int: $(info.name) = $lb;")
            elseif typemin(T) < lb && ub < typemax(T)
                print(io, "var $lb .. $ub: $(info.name);")
            else
                print(io, "var int: $(info.name);")
                if ub < typemax(T)
                    constraint_lines *= "constraint int_lt($(info.name), $ub);\n"
                end
                if typemin(T) < lb
                    constraint_lines *= "constraint int_gt($(info.name), $lb);\n"
                end
            end
        else
            if lb == ub
                print(io, "var float: $(info.name) = $lb;")
            elseif typemin(T) < lb && ub < typemax(T)
                print(io, "var $lb .. $ub: $(info.name);")
            else
                print(io, "var float: $(info.name);")
                if ub < typemax(T)
                    constraint_lines *= "constraint $(info.name) <= $ub;\n"
                end
                if typemin(T) < lb
                    constraint_lines *= "constraint $(info.name) >= $lb;\n"
                end
            end
        end
        println(io)
    end
    return variables, constraint_lines
end

function _to_string(
    variables,
    f::MOI.VariableIndex;
    include_constant::Bool = false,
)
    return variables[f].name
end

function _to_string(
    variables,
    f::MOI.VectorOfVariables;
    include_constant::Bool = false,
)
    inner = join([_to_string(variables, v) for v in f.variables], ", ")
    return "[$inner]"
end

function _to_string(
    variables,
    f::MOI.ScalarAffineFunction;
    include_constant::Bool = false,
)
    ret = ""
    prefix = ""
    for t in f.terms
        name = _to_string(variables, t.variable)
        ret *= "$(prefix)$(t.coefficient)*$name"
        prefix = " + "
    end
    if include_constant
        ret *=" + $(f.constant)"
    end
    return ret
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{MOI.AllDifferent},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        str = _to_string(variables, f)
        println(io, "constraint alldifferent(", str, ");")
    end
    return
end

_sense(s::MOI.LessThan) = " <= "
_sense(s::MOI.GreaterThan) = " >= "
_sense(s::MOI.EqualTo) = " = "

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
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        print(io, "constraint ", _to_string(variables, f))
        println(io, _sense(s), _rhs(s) - f.constant, ";")
    end
    return
end

function _write_predicates(io, model)
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if S == MOI.AllDifferent
            println(io, "include \"alldifferent.mzn\";")
        end
    end
    return
end

function Base.write(io::IO, model::Model{T}) where {T}
    MOI.FileFormats.create_unique_variable_names(
        model,
        false,
        [
            s -> match(r"^[^a-zA-Z]", s) !== nothing ? "x" * s : s,
            s -> replace(s, r"[^A-Za-z0-9_]" => "_"),
        ],
    )
    _write_predicates(io, model)
    variables, constraint_lines = _write_variables(io, model)
    print(io, constraint_lines)
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
        print(io, _to_string(variables, f; include_constant = true))
        println(io, ";")
    end
    return
end
