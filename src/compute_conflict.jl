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

# Locate the findMUS solver configuration (`findmus.msc`). FindMUS_jll provides
# it; `JULIA_FINDMUS_MSC` overrides that with a locally built config (the failure
# test points it at a config whose binary is missing).
function _findmus_msc()
    return get(ENV, "JULIA_FINDMUS_MSC", FindMUS_jll.findmus_msc)
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

# The findMUS invocation. `--named-only` scopes the search to our annotated
# constraints; `-g --soft-defines` keep bound/functional constraints from being
# absorbed into variable domains (otherwise they vanish from the MUS);
# `--no-leftover` suppresses any non-minimal candidate emitted on timeout, so any
# reported MUS is guaranteed minimal; `--paramset mzn` selects MiniZinc-level
# output so each member's annotation surfaces as its `expression_name`. Kept a
# pure builder so the exact flag set stays unit-testable without findMUS present.
function _findmus_cmd(
    exe,
    msc::AbstractString,
    filename::AbstractString,
    overall_ms::Integer,
    sub_ms::Integer,
)
    return `$(exe) --solver $(msc) --named-only -g --soft-defines --paramset mzn --output-json -n 1 -t $(overall_ms) --no-leftover --subsolver org.chuffed.chuffed --subsolver-timelimit $(sub_ms) $(filename)`
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
    # Per-check subsolver budget. findMUS first proves the whole model UNSAT
    # within this limit (it aborts with a failure otherwise) and treats a
    # subsolver timeout as SAT, so it must be generous enough for the full-model
    # check while staying bounded.
    sub_ms = clamp(div(overall_ms, 2), 1_000, 30_000)
    solver_path = _findmus_solver_path(msc)
    out_file = joinpath(dir, "stdout.txt")
    err_file = joinpath(dir, "stderr.txt")
    killed = Ref(false)
    failure = try
        _minizinc_exe() do exe
            cmd = addenv(
                _findmus_cmd(exe, msc, filename, overall_ms, sub_ms),
                "MZN_SOLVER_PATH" => solver_path,
            )
            proc = run(
                pipeline(cmd; stdout = out_file, stderr = err_file);
                wait = false,
            )
            # findMUS's own `-t` should fire first; this backstops a hung driver.
            # `kill` targets the `minizinc` process; any findMUS/Chuffed children
            # are independently bounded by `-t` and `--subsolver-timelimit`, so a
            # driver-only kill cannot leave an unbounded process behind. The output
            # captured below is only trusted when a complete MUS block is present,
            # which this killed path never produces.
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
            # A process that exited cleanly is a success even if the backstop
            # timer fired in the narrow race before `wait` returned (`kill` on an
            # already-finished process is a no-op).
            if success(proc)
                return nothing
            elseif killed[]
                return "findMUS exceeded the wall-clock limit and was terminated"
            end
            return "findMUS exited with a non-zero status"
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
# `%%%mzn-json-start … %%%mzn-json-end` block (emitted by `--output-json`, which
# `_findmus_cmd` always passes), keeping only tokens we emitted. `eachmatch`
# scans each line for every `expression_name`, so a compacted block (more than
# one field on a line, as `--json-stream` would emit) is not under-reported.
# `-n 1` + `--no-leftover` yield at most one block; multiple would simply union.
function _parse_findmus_tokens(output::AbstractString, known::Set{String})
    found = Set{String}()
    in_block = false
    for line in eachline(IOBuffer(output))
        if occursin("%%%mzn-json-start", line)
            in_block = true
        elseif occursin("%%%mzn-json-end", line)
            in_block = false
        elseif in_block
            for m in eachmatch(r"\"expression_name\"\s*:\s*\"([^\"]*)\"", line)
                if m[1] in known
                    push!(found, m[1])
                end
            end
        end
    end
    return found
end

# Map a findMUS run's captured output to a conflict status and the set of
# conflicting constraints. Kept free of I/O (the subprocess lives in
# `_run_findmus`) so it can be unit-tested with canned findMUS reports.
#
# A reported MUS is authoritative and minimal (`--no-leftover`), so its tokens
# are mapped to constraints first and trusted even if the process later exited
# non-zero. With no attributable MUS, findMUS reaches this cleanly for a
# satisfiable foreground, for a conflict lying entirely in unnamed/background
# constraints ("Background is not satisfiable"), and for a timeout whose
# non-minimal leftover `--no-leftover` suppressed. The MiniZinc driver currently
# returns success for all of these; we additionally recognise findMUS's own
# messages so the result is robust to a driver that propagates findMUS's
# (non-zero) exit. The non-conflict status is NO_CONFLICT_FOUND, never
# NO_CONFLICT_EXISTS, which would falsely assert the model is feasible.
function _classify_conflict(
    output::AbstractString,
    errors::AbstractString,
    failure::Union{Nothing,String},
    tokens::Dict{MOI.ConstraintIndex,String},
)
    conflict = Set{MOI.ConstraintIndex}()
    conflicted = _parse_findmus_tokens(output, Set(values(tokens)))
    for (ci, token) in tokens
        if token in conflicted
            push!(conflict, ci)
        end
    end
    if !isempty(conflict)
        return MOI.CONFLICT_FOUND, conflict
    end
    # findMUS's two benign non-conflict messages on stderr: a satisfiable
    # foreground (`EXIT_SUCCESS`), and a conflict lying only in unnamed/background
    # constraints. Tracked against findMUS v0.7.0; re-verify the spellings when
    # bumping FindMUS_jll (`test_compute_conflict_feasible` covers the satisfiable
    # path).
    benign =
        occursin("Model is Satisfiable", errors) ||
        occursin("Background is not satisfiable", errors)
    if failure === nothing || benign
        return MOI.NO_CONFLICT_FOUND, conflict
    end
    return error(
        "findMUS failed to compute a conflict: ",
        failure,
        ". If findMUS timed out, raise the limit with `MOI.TimeLimitSec`.\n",
        strip(string(errors, "\n", output)),
    )
end

"""
    MOI.compute_conflict!(model::Optimizer)

Compute a minimal conflicting subset of constraints (an Irreducible
Inconsistent Subsystem) for an infeasible model using findMUS, and record it
for [`MOI.ConstraintConflictStatus`](@ref). Call after `optimize!` returns
`INFEASIBLE`.

The outcome is reported through [`MOI.ConflictStatus`](@ref):
- `CONFLICT_FOUND` — a minimal conflict was found; its members read
  `IN_CONFLICT` from `ConstraintConflictStatus`.
- `NO_CONFLICT_FOUND` — no conflict could be attributed to the model's
  constraints (the conflict involves only variable bounds, lies outside the
  model, or could not be isolated in the time limit). This does **not** assert
  the model is feasible.

findMUS is provided by the `FindMUS_jll` dependency, so no setup is required; a
genuine findMUS failure throws an `ErrorException`.

Conflicts cover modeling constraints only: MiniZinc folds variable bounds into
variable declarations, so a bound is never reported `IN_CONFLICT`. The reported
conflict is guaranteed minimal; on timeout no conflict is reported rather than a
possibly non-minimal one. Conflict analysis uses the Chuffed subsolver and is
bounded by [`MOI.TimeLimitSec`](@ref) (default 60 seconds).

See also [`MOI.ConflictStatus`](@ref) and
[`MOI.ConstraintConflictStatus`](@ref).
"""
function MOI.compute_conflict!(dest::Optimizer)
    msc = _findmus_msc()
    empty!(dest.conflict_constraints)
    dest.conflict_status = MOI.COMPUTE_CONFLICT_NOT_CALLED
    dest.inner.ext[:conflict_annotate] = true
    output, errors, failure, tokens = try
        _run_findmus(dest, msc)
    finally
        delete!(dest.inner.ext, :conflict_annotate)
        delete!(dest.inner.ext, :conflict_tokens)
    end
    status, conflict = _classify_conflict(output, errors, failure, tokens)
    for ci in conflict
        push!(dest.conflict_constraints, ci)
    end
    dest.conflict_status = status
    return
end

MOI.get(model::Optimizer, ::MOI.ConflictStatus) = model.conflict_status

# findMUS reports a single minimal conflict (`-n 1`), so there is at most one.
function MOI.get(model::Optimizer, ::MOI.ConflictCount)
    return model.conflict_status == MOI.CONFLICT_FOUND ? 1 : 0
end

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
    # `attr.conflict_index` must name one of the (at most one) computed
    # conflicts, and `ci` must belong to this model.
    MOI.check_conflict_index_bounds(model, attr)
    MOI.throw_if_not_valid(model.inner, ci)
    # Only annotated modeling constraints can be IN_CONFLICT; variable bounds
    # (folded into variable declarations) and indices outside the conflict read
    # NOT_IN_CONFLICT. See `compute_conflict!`.
    if ci in model.conflict_constraints
        return MOI.IN_CONFLICT
    end
    return MOI.NOT_IN_CONFLICT
end
