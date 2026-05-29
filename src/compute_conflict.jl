# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Conflict (IIS) support via findMUS.
#
# findMUS (https://gitlab.com/minizinc/FindMUS) is a Minimal Unsatisfiable Subset
# tool that ships as a MiniZinc pseudo-solver: it is invoked as
# `minizinc --solver findMUS <model.mzn>` and, given an unsatisfiable model,
# reports a minimal subset of constraints whose conjunction is unsatisfiable.
#
# We make each MOI constraint individually identifiable by stamping it with a
# unique string annotation in `write.jl` (`constraint (..) :: "c<k>";`). findMUS
# surfaces that string as the constraint's `expression_name` in its
# `--output-json` report, which we map back to the originating `ConstraintIndex`.

# Locate the findMUS solver configuration (`findmus.msc`). During development,
# set `JULIA_FINDMUS_MSC` to the absolute path of a built `findmus.msc`. Returns
# `nothing` when findMUS is unavailable.
function _findmus_msc()
    msc = get(ENV, "JULIA_FINDMUS_MSC", nothing)
    if msc !== nothing && isfile(msc)
        return msc
    end
    return nothing
end

# Directories to expose to the MiniZinc driver via `MZN_SOLVER_PATH` so it can
# resolve both findMUS itself and the Chuffed subsolver it shells out to.
function _findmus_solver_path(msc::AbstractString)
    sep = Sys.iswindows() ? ';' : ':'
    dirs = String[dirname(abspath(msc))]
    chuffed = Chuffed()
    if chuffed !== nothing
        push!(dirs, dirname(chuffed))
    end
    existing = get(ENV, "MZN_SOLVER_PATH", "")
    if !isempty(existing)
        push!(dirs, existing)
    end
    return join(dirs, sep)
end

# Run findMUS on the (annotated) inner model. Returns
# `(stdout, stderr, failure, tokens)` where `failure` is `nothing` on a clean run
# or a human-readable reason otherwise, and `tokens` is the
# `ConstraintIndex -> token` table produced by the annotated write.
function _run_findmus(dest::Optimizer, msc::AbstractString)
    dir = mktempdir()
    filename = joinpath(dir, "model.mzn")
    open(io -> write(io, dest.inner), filename, "w")
    tokens = get(
        dest.inner.ext,
        :conflict_tokens,
        Dict{MOI.ConstraintIndex,String}(),
    )
    overall_ms = round(Int, 1_000 * something(dest.time_limit_sec, 60.0))
    sub_ms = clamp(div(overall_ms, 2), 1_000, 30_000)
    solver_path = _findmus_solver_path(msc)
    out_file = joinpath(dir, "stdout.txt")
    err_file = joinpath(dir, "stderr.txt")
    killed = Ref(false)
    # `--named-only` scopes the search to our annotated constraints; `-g
    # --soft-defines` keep bound/functional constraints from being absorbed into
    # variable domains (otherwise they vanish from the MUS); `--no-leftover`
    # suppresses any non-minimal candidate emitted on timeout, so any reported
    # MUS is guaranteed minimal.
    failure = try
        _minizinc_exe() do exe
            cmd = `$(exe) --solver $(msc) --named-only -g --soft-defines --paramset mzn --output-json -n 1 -t $(overall_ms) --no-leftover --subsolver org.chuffed.chuffed --subsolver-timelimit $(sub_ms) $(filename)`
            cmd = addenv(cmd, "MZN_SOLVER_PATH" => solver_path)
            proc = run(
                pipeline(cmd; stdout = out_file, stderr = err_file);
                wait = false,
            )
            # findMUS's own `-t` should fire first; this backstops a hung process.
            timer = Timer(overall_ms / 1_000 + 30.0) do _t
                if process_running(proc)
                    killed[] = true
                    kill(proc)
                end
            end
            try
                wait(proc)
            finally
                close(timer)
            end
            if killed[]
                return "findMUS exceeded the wall-clock limit and was terminated"
            elseif !success(proc)
                return "findMUS exited with a non-zero status"
            end
            return nothing
        end
    catch err
        err isa InterruptException && rethrow(err)
        "findMUS could not be run ($(sprint(showerror, err)))"
    end
    output = isfile(out_file) ? read(out_file, String) : ""
    errors = isfile(err_file) ? read(err_file, String) : ""
    return output, errors, failure, tokens
end

# Collect the `expression_name` tokens findMUS reports inside its
# `%%%mzn-json-start … %%%mzn-json-end` block, keeping only tokens we emitted.
# This relies on findMUS pretty-printing one JSON field per line
# (`lib/Types.cpp::getJSONSummary`) and on `-n 1` + `--no-leftover` yielding at
# most one block; a switch to compact JSON output upstream would break this
# line-based scan.
function _parse_findmus_tokens(output::AbstractString, known::Set{String})
    found = Set{String}()
    in_block = false
    for line in eachline(IOBuffer(output))
        if occursin("%%%mzn-json-start", line)
            in_block = true
        elseif occursin("%%%mzn-json-end", line)
            in_block = false
        elseif in_block
            m = match(r"\"expression_name\"\s*:\s*\"([^\"]*)\"", line)
            if m !== nothing && m[1] in known
                push!(found, m[1])
            end
        end
    end
    return found
end

"""
    MOI.compute_conflict!(model::Optimizer)

Compute a minimal conflicting subset of constraints (an Irreducible Infeasible
Subset) for an infeasible model using findMUS, and record it for
[`MOI.ConstraintConflictStatus`](@ref). Call after `optimize!` returns
`INFEASIBLE`.

The outcome is reported through [`MOI.ConflictStatus`](@ref):
- `CONFLICT_FOUND` — a minimal conflict was found; its members read
  `IN_CONFLICT` from `ConstraintConflictStatus`.
- `NO_CONFLICT_FOUND` — no conflict could be attributed to the model's
  constraints (the conflict involves only variable bounds, lies outside the
  model, or could not be isolated in the time limit). This does **not** assert
  the model is feasible.

findMUS must be available: set the `JULIA_FINDMUS_MSC` environment variable to a
built `findmus.msc` (a future `FindMUS_jll` dependency can provide it). If it is
absent, an `ArgumentError` naming `compute_conflict!` is thrown so callers can
tell a missing capability apart from a failed computation; a genuine findMUS
failure throws an `ErrorException`.

Conflicts cover modeling constraints only: MiniZinc folds variable bounds into
variable declarations, so a bound is never reported `IN_CONFLICT`. The reported
conflict is guaranteed minimal; on timeout no conflict is reported rather than a
possibly non-minimal one. Conflict analysis uses the Chuffed subsolver and is
bounded by [`MOI.TimeLimitSec`](@ref) (default 60 seconds).
"""
function MOI.compute_conflict!(dest::Optimizer)
    msc = _findmus_msc()
    if msc === nothing
        throw(
            ArgumentError(
                "`compute_conflict!` requires findMUS, which is not available. " *
                "Set the `JULIA_FINDMUS_MSC` environment variable to a built " *
                "`findmus.msc` (a `FindMUS_jll` dependency can provide this).",
            ),
        )
    end
    empty!(dest.conflict_constraints)
    dest.conflict_status = MOI.COMPUTE_CONFLICT_NOT_CALLED
    dest.inner.ext[:conflict_annotate] = true
    output, errors, failure, tokens = try
        _run_findmus(dest, msc)
    finally
        delete!(dest.inner.ext, :conflict_annotate)
        delete!(dest.inner.ext, :conflict_tokens)
    end
    # A reported MUS is authoritative and minimal (`--no-leftover`), so map its
    # tokens to constraints first and trust it even if the process later exited
    # non-zero.
    conflicted = _parse_findmus_tokens(output, Set(values(tokens)))
    for (ci, token) in tokens
        if token in conflicted
            push!(dest.conflict_constraints, ci)
        end
    end
    if !isempty(dest.conflict_constraints)
        dest.conflict_status = MOI.CONFLICT_FOUND
        return
    end
    # No attributable MUS. findMUS reaches this cleanly for a satisfiable
    # foreground, for a conflict that lies entirely in unnamed/background
    # constraints ("Background is not satisfiable"), and for a timeout whose
    # non-minimal leftover `--no-leftover` suppressed. The MiniZinc driver
    # currently returns success for all of these; we additionally recognise
    # findMUS's own messages so the result is robust to a driver that propagates
    # findMUS's (non-zero) exit. Report NO_CONFLICT_FOUND — never
    # NO_CONFLICT_EXISTS, which would falsely assert the model is feasible.
    benign =
        occursin("Model is Satisfiable", errors) ||
        occursin("Background is not satisfiable", errors)
    if failure === nothing || benign
        dest.conflict_status = MOI.NO_CONFLICT_FOUND
        return
    end
    return error(
        "findMUS failed to compute a conflict: ",
        failure,
        ".\n",
        strip(string(errors, "\n", output)),
    )
end

MOI.get(model::Optimizer, ::MOI.ConflictStatus) = model.conflict_status

function MOI.get(
    model::Optimizer,
    attr::MOI.ConstraintConflictStatus,
    ci::MOI.ConstraintIndex,
)
    if model.conflict_status == MOI.COMPUTE_CONFLICT_NOT_CALLED
        throw(
            MOI.GetAttributeNotAllowed(attr, "Call `compute_conflict!` first."),
        )
    end
    if ci in model.conflict_constraints
        return MOI.IN_CONFLICT
    end
    return MOI.NOT_IN_CONFLICT
end
