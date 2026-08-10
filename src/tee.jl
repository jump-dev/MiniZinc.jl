# Copyright (c) 2022 MiniZinc.jl contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# An `IO` that mirrors everything written to it into two sinks. Used to stream
# solver output live to the user's terminal while still capturing it to the
# files that the result-parsing and error paths read. `run(pipeline(...))`
# accepts any writable `IO` as a redirect target: Base copies the child's
# output through `unsafe_write`, and `write(::IO, ::UInt8)` is the fallback
# that generic `print`/`write` calls reduce to.
struct _Tee{A<:IO,B<:IO} <: IO
    a::A
    b::B
end

function Base.write(t::_Tee, byte::UInt8)
    write(t.a, byte)
    return write(t.b, byte)  # We could return either .a or .b
end

function Base.unsafe_write(t::_Tee, p::Ptr{UInt8}, n::UInt)
    unsafe_write(t.a, p, n)
    return unsafe_write(t.b, p, n)  # We could return either .a or .b
end

function Base.flush(t::_Tee)
    flush(t.a)
    flush(t.b)
    return
end

Base.iswritable(t::_Tee) = iswritable(t.a) && iswritable(t.b)
