#=
- https://github.com/JuliaLang/julia/issues/13984
- https://github.com/JuliaDiff/ReverseDiff.jl/issues/70
- https://github.com/JuliaDiff/ReverseDiff.jl/pull/71
- https://github.com/yuyichao/FunctionWrappers.jl
=#

using BenchmarkTools
using FunctionWrappers: FunctionWrapper


abstract type T0 end

struct T1 <: T0
    x::Int
end

struct T2 <: T0
    x::Int
end

p1(t::T1) = t.x * 2
p1(t::T2) = t.x * 3

p2(t::T0) = t.x * 2

function run(v::Vector{T}) where T
    println("\tcalling p1")
    @btime foreach(p1, $v)

    println("\tcalling p2")
    @btime foreach(p2, $v)

    println("\tcalling closures")
    cbs = [() -> p1(x) for x in v]
    @btime for cb in $cbs
        cb()
    end

    println("\tcalling FunctionWrappers")
    wcbs = [FunctionWrapper{Nothing, Tuple{}}(() -> p1(x)) for x in v]
    @btime for cb in $wcbs
        cb()
    end
end

const v0 = [rand([T1,T2])(rand(Int)) for _ in 1:10]
const v1 = [T1(rand(Int)) for _ in 1:10]
const v2 = [T2(rand(Int)) for _ in 1:10]

println("Run on Vector{T0} (T0 is abstract type)")
run(v0)
println("Run on Vector{T1} (T1 is concrete type)")
run(v1)
println("Run on Vector{T2} (T2 is concrete type)")
run(v2)
