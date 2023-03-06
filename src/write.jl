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
    variables,
    f::MOI.VectorOfVariables,
    s::Union{
        MOI.AllDifferent,
        MOI.CountDistinct,
        MOI.CountGreaterThan,
        MOI.Cumulative,
        MOI.Circuit,
    },
)
    mzn = MiniZincSet(s)
    strs = [_to_string(variables, f.variables[field]) for field in mzn.fields]
    println(io, "constraint $(mzn.name)(", join(strs, ", "), ");")
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.Reified{S2},
) where {
    S2<:Union{
        MOI.AllDifferent,
        MOI.CountDistinct,
        MOI.CountGreaterThan,
        MOI.Cumulative,
    },
}
    mzn = MiniZincSet(s.set)
    z = _to_string(variables, f.variables[1])
    strs = [_to_string(variables, f.variables[i.+1]) for i in mzn.fields]
    println(io, "constraint $z <-> $(mzn.name)(", join(strs, ", "), ");")
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
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.CountBelongs,
)
    n = _to_string(variables, f.variables[1])
    x = _to_string(variables, f.variables[2:end])
    v = string("{", join(sort([i for i in s.set]), ", "), "}")
    println(io, "constraint among(", n, ", ", x, ", ", v, ");")
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.Reified{MOI.CountBelongs},
)
    b = _to_string(variables, f.variables[1])
    n = _to_string(variables, f.variables[2])
    x = _to_string(variables, f.variables[3:end])
    v = string("{", join(sort([i for i in s.set.set]), ", "), "}")
    println(io, "constraint $b <-> among(", n, ", ", x, ", ", v, ");")
    return
end

function _write_at_least(io, variables, f, s, offset)
    print(io, "at_least(", s.n, ", [")
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
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.CountAtLeast,
)
    print(io, "constraint ")
    _write_at_least(io, variables, f, s, 0)
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.Reified{MOI.CountAtLeast},
)
    b = _to_string(variables, f.variables[1])
    print(io, "constraint $b <-> ")
    _write_at_least(io, variables, f, s.set, 1)
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.BinPacking,
)
    x = _to_string(variables, f.variables)
    print(io, "constraint bin_packing(", s.capacity, ", ", x, ", ")
    println(io, s.weights, ");")
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.Reified{<:MOI.BinPacking},
)
    b = _to_string(variables, f.variables[1])
    x = _to_string(variables, f.variables[2:end])
    print(io, "constraint $b <-> bin_packing(", s.set.capacity, ", ", x, ", ")
    println(io, s.set.weights, ");")
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.Path,
)
    s1 = _to_string(variables, f.variables[1])
    t1 = _to_string(variables, f.variables[2])
    ns = _to_string(variables, f.variables[2 .+ (1:s.N)])
    es = _to_string(variables, f.variables[(2+s.N).+(1:s.E)])
    print(io, "constraint path(", s.N, ", ", s.E, ", ", s.from, ", ")
    println(io, s.to, ", ", s1, ", ", t1, ", ", ns, ", ", es, ");")
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.Table,
)
    x = _to_string(variables, f.variables)
    print(io, "constraint table(", x, ", [|")
    for i in 1:size(s.table, 1)
        print(io, " ", join(s.table[i, :], ", "), " |")
    end
    println(io, "]);")
    return
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorOfVariables,
    s::MOI.Reified{<:MOI.Table},
)
    b = _to_string(variables, f.variables[1])
    x = _to_string(variables, f.variables[2:end])
    print(io, "constraint $b <-> table(", x, ", [|")
    for i in 1:size(s.set.table, 1)
        print(io, " ", join(s.set.table[i, :], ", "), " |")
    end
    println(io, "]);")
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
    variables,
    f::MOI.ScalarAffineFunction{T},
    s::Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}},
) where {T}
    print(io, "constraint ", _to_string(variables, f))
    println(io, _sense(s), _rhs(s) - f.constant, ";")
    return
end

function _to_epigraph(variables, f::MOI.VectorAffineFunction)
    @assert MOI.output_dimension(f) == 2
    f_z, x = MOI.Utilities.eachscalar(f)
    z = convert(MOI.VariableIndex, f_z)
    return _to_string(variables, z), _to_string(variables, x)
end

function _write_constraint(
    io::IO,
    variables,
    f::MOI.VectorAffineFunction{T},
    s::MOI.Reified{S2},
) where {T,S2<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}}}
    z, x = _to_epigraph(variables, f)
    fx = string(x, _sense(s.set), _rhs(s.set) - f.constants[2])
    println(io, "constraint $z <-> $fx;")
    return
end

function _write_predicates(io::IO, model)
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if S == MOI.AllDifferent || S == MOI.Reified{MOI.AllDifferent}
            println(io, "include \"alldifferent.mzn\";")
        elseif S <: MOI.BinPacking || S <: MOI.Reified{<:MOI.BinPacking}
            println(io, "include \"bin_packing.mzn\";")
        elseif S == MOI.Circuit  # Reified unsupported by MiniZinc
            println(io, "include \"circuit.mzn\";")
        elseif S == MOI.CountAtLeast || S == MOI.Reified{MOI.CountAtLeast}
            println(io, "include \"at_least.mzn\";")
        elseif S == MOI.CountBelongs || S == MOI.Reified{MOI.CountBelongs}
            println(io, "include \"among.mzn\";")
        elseif S == MOI.CountDistinct || S == MOI.Reified{MOI.CountDistinct}
            println(io, "include \"nvalue.mzn\";")
        elseif S == MOI.CountGreaterThan ||
               S == MOI.Reified{MOI.CountGreaterThan}
            println(io, "include \"count_gt.mzn\";")
        elseif S == MOI.Cumulative || S == MOI.Reified{MOI.Cumulative}
            println(io, "include \"cumulative.mzn\";")
        elseif S == MOI.Path  # Reified unsupported by MiniZinc
            println(io, "include \"path.mzn\";")
        elseif S <: MOI.Table || S <: MOI.Reified{<:MOI.Table}
            println(io, "include \"table.mzn\";")
        end
    end
    return
end

function _write_nlp_constraint(io::IO, variables, expr::Expr)
    print(io, "constraint ")
    if Meta.isexpr(expr, :call, 3)
        @assert expr.args[1] in (:(<=), :(>=), :(<), :(>), :(==))
        _write_expression(io, variables, expr.args[2])
        rhs = expr.args[3]
        if isone(rhs)
            println(io, " $(expr.args[1]) true;")
        else
            @assert iszero(rhs)
            println(io, " $(expr.args[1]) false;")
        end
    else
        @assert Meta.isexpr(expr, :comparison, 5)
        error("Two sided not supported")
    end
    return
end

function _write_logical_expression(io::IO, variables, expr::Expr)
    ops = Dict(:|| => "\\/", :&& => "/\\")
    op = get(ops, expr.head, nothing)
    @assert op !== nothing
    print(io, "(")
    _write_expression(io, variables, expr.args[1])
    for i in 2:length(expr.args)
        print(io, " ", op, " ")
        _write_expression(io, variables, expr.args[i])
    end
    print(io, ")")
    return
end

_INFIX_OPS = Dict(
    :- => "-",
    :+ => "+",
    :* => "*",
    :(<) => "<",
    :(>) => ">",
    :(<=) => "<=",
    :(>=) => ">=",
    :(=>) => "->",
    :⊻ => "xor",
)

_PREFIX_OPS = Dict(:(!) => "not")

function _write_call_expression(io::IO, variables, expr::Expr)
    op = get(_INFIX_OPS, expr.args[1], nothing)
    if op !== nothing
        print(io, "(")
        _write_expression(io, variables, expr.args[2])
        for i in 3:length(expr.args)
            print(io, " ", op, " ")
            _write_expression(io, variables, expr.args[i])
        end
        print(io, ")")
    else
        op = get(_PREFIX_OPS, expr.args[1], nothing)
        @assert op !== nothing
        print(io, op, "(")
        _write_expression(io, variables, expr.args[2])
        print(io, ")")
    end
    return
end

function _write_expression(io::IO, variables, expr::Expr)
    if Meta.isexpr(expr, :ref)
        @assert expr.args[1] == :x
        _write_expression(io, variables, expr.args[2])
        return
    end
    if Meta.isexpr(expr, :||) || Meta.isexpr(expr, :&&)
        _write_logical_expression(io, variables, expr)
    else
        @assert Meta.isexpr(expr, :call)
        _write_call_expression(io, variables, expr)
    end
    return
end

function _write_expression(io::IO, variables, x::MOI.VariableIndex)
    print(io, _to_string(variables, x))
    return
end

function _write_expression(io::IO, _, x::Real)
    print(io, isinteger(x) ? round(Int, x) : x)
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
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            f = MOI.get(model, MOI.ConstraintFunction(), ci)
            s = MOI.get(model, MOI.ConstraintSet(), ci)
            _write_constraint(io, variables, f, s)
        end
    end
    nlp_block = get(model.ext, :nlp_block, nothing)
    if nlp_block !== nothing
        MOI.initialize(nlp_block.evaluator, [:ExprGraph])
        for i in 1:length(nlp_block.constraint_bounds)
            expr = MOI.constraint_expr(nlp_block.evaluator, i)
            _write_nlp_constraint(io, variables, expr)
        end
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
