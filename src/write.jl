# Copyright (c) 2022 MiniZinc.jl contributors
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
            print(io, "var bool: $(info.name);")
            if ub == 0
                constraint_lines *= "constraint bool_eq($(info.name), false);\n"
            elseif lb == 1
                constraint_lines *= "constraint bool_eq($(info.name), true);\n"
            end
        elseif info.is_int
            if typemin(T) < lb && ub < typemax(T)
                print(io, "var $lb .. $ub: $(info.name);")
            else
                print(io, "var int: $(info.name);")
                if ub < typemax(T)
                    constraint_lines *= "constraint int_le($(info.name), $ub);\n"
                end
                if typemin(T) < lb
                    constraint_lines *= "constraint int_ge($(info.name), $lb);\n"
                end
            end
        else
            if typemin(T) < lb && ub < typemax(T)
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

_to_string(variables, f::MOI.VariableIndex; kwargs...) = variables[f].name

function _to_string(variables, f::Vector{MOI.VariableIndex}; kwargs...)
    return string("[", join([_to_string(variables, v) for v in f], ", "), "]")
end

function _to_string(variables, f::MOI.VectorOfVariables; kwargs...)
    return _to_string(variables, f.variables; kwargs...)
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
        ret *= " + $(f.constant)"
    end
    return ret
end

struct MiniZincSet <: MOI.AbstractSet
    name::String
    fields::Vector{Union{Int,UnitRange{Int}}}
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{
        <:Union{
            MOI.AllDifferent,
            MOI.CountDistinct,
            MOI.CountGreaterThan,
            MOI.Cumulative,
            MOI.Circuit,
        },
    },
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        mzn = MiniZincSet(s)
        strs =
            [_to_string(variables, f.variables[field]) for field in mzn.fields]
        println(io, "constraint $(mzn.name)(", join(strs, ", "), ");")
    end
    return
end

function MiniZincSet(set::MOI.AllDifferent)
    return MiniZincSet("alldifferent", [1:set.dimension])
end

function MiniZincSet(set::MOI.CountDistinct)
    return MiniZincSet("nvalue", [1, 2:set.dimension])
end

function MiniZincSet(set::MOI.CountGreaterThan)
    return MiniZincSet("count_gt", [3:set.dimension, 2, 1])
end

function MiniZincSet(set::MOI.Cumulative)
    d = set.dimension
    n = div(d - 1, 3)
    return MiniZincSet("cumulative", [1:n, n .+ (1:n), 2n .+ (1:n), d])
end

function MiniZincSet(set::MOI.Circuit)
    return MiniZincSet("circuit", [1:set.dimension])
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{MOI.CountBelongs},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        n = _to_string(variables, f.variables[1])
        x = _to_string(variables, f.variables[2:end])
        v = string("{", join(sort([i for i in s.set]), ", "), "}")
        println(io, "constraint among(", n, ", ", x, ", ", v, ");")
    end
    return
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{MOI.CountAtLeast},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        offset = 0
        print(io, "constraint at_least(", s.n, ", [")
        prefix = ""
        for p in s.partitions
            print(io, prefix, "{")
            inner_prefix = ""
            for i in 1:p
                print(
                    io,
                    inner_prefix,
                    _to_string(variables, f.variables[offset+i]),
                )
                inner_prefix = ", "
            end
            print(io, "}")
            offset += p
            prefix = ", "
        end
        prefix = ""
        print(io, "], {")
        for i in sort([i for i in s.set])
            print(io, prefix, i)
            prefix = ", "
        end
        println(io, "});")
    end
    return
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{<:MOI.BinPacking},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        x = _to_string(variables, f.variables)
        print(io, "constraint bin_packing(", s.capacity, ", ", x, ", ")
        println(io, s.weights, ");")
    end
    return
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{MOI.Path},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        s = _to_string(variables, f.variables[1])
        t = _to_string(variables, f.variables[2])
        ns = _to_string(variables, f.variables[2 .+ (1:set.N)])
        es = _to_string(variables, f.variables[(2+set.N).+(1:set.E)])
        print(io, "constraint path(", set.N, ", ", set.E, ", ", set.from, ", ")
        println(io, set.to, ", ", s, ", ", t, ", ", ns, ", ", es, ");")
    end
    return
end

function _write_constraint(
    io::IO,
    model,
    variables,
    F::Type{MOI.VectorOfVariables},
    S::Type{<:MOI.Table},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        x = _to_string(variables, f.variables)
        print(io, "constraint table(", x, ", [|")
        for i in 1:size(set.table, 1)
            print(io, " ", join(set.table[i, :], ", "), " |")
        end
        println(io, "]);")
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
        elseif S <: MOI.BinPacking
            println(io, "include \"bin_packing.mzn\";")
        elseif S == MOI.Circuit
            println(io, "include \"circuit.mzn\";")
        elseif S == MOI.CountAtLeast
            println(io, "include \"at_least.mzn\";")
        elseif S == MOI.CountBelongs
            println(io, "include \"among.mzn\";")
        elseif S == MOI.CountDistinct
            println(io, "include \"nvalue.mzn\";")
        elseif S == MOI.CountGreaterThan
            println(io, "include \"count_gt.mzn\";")
        elseif S == MOI.Cumulative
            println(io, "include \"cumulative.mzn\";")
        elseif S == MOI.Path
            println(io, "include \"path.mzn\";")
        elseif S <: MOI.Table
            println(io, "include \"table.mzn\";")
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
