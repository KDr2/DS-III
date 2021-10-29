using IRTools
# using MacroTools

mutable struct Instruction{F}
    fun::F
    input::Tuple
    output
end

const Tape = Vector{Instruction}

mutable struct Box{T}
    val::T
    tape::Tape
end

val(x) = x
val(x::Box) = x.val
box(x, t::Tape) = Box(x, t)

tape(x) = nothing
tape(x::Box) = x.tape
tape(x::Tape) = x
function tape(x::Tuple)
    for i in x
        tape(i) != nothing && return tape(i)
    end
end
result(t::Tape) = isempty(t) ? nothing : val(t[end].output)

function Base.show(io::IO, instruction::Instruction)
    println(io, "Instruction($(instruction.fun))")
end

function Base.show(io::IO, tp::Tape)
    println("$(length(tp))-element Tape:")
    i = 1
    for instruction in tp
        print(io, "$i => ")
        show(io, instruction)
        i += 1
    end
end

function (ins::Instruction{F})() where F
    output = ins.fun(map(val, ins.input)...)
    ins.output.val = output
end

function run(tape::Tape, args...)
    input = map(x-> box(x, tape), args)
    tape[1].input = input
    for ins in tape
        ins()
    end
end

function run_and_record!(tape::Tape, f, args...)
    output = f(map(val, args)...)
    output = box(output, tape)
    ins = Instruction(f, args, output)
    push!(tape, ins)
    return output
end


function intercept(ir)
    ir == nothing && return
    tape = pushfirst!(ir, IRTools.xcall(@__MODULE__, :Tape))
    for (x, st) in ir
        x == tape && continue
        Meta.isexpr(st.expr, :call) || continue
        ir[x] = IRTools.xcall(@__MODULE__, :run_and_record!, tape, st.expr.args...)
    end
    IRTools.return!(ir, tape)
    return ir
end



IRTools.@dynamo function record(a...)
    ir = IRTools.IR(a...)
    return intercept(ir)
end


### Testing

f1(x) = x + 1
f2(x) = x * 2
f3(x) = x ^ 3

# debug
#=
f(x) = f1(x)
ir = @code_ir f(2)
ir2 = intercept(ir)
@show ir
@show ir2, IRTools.evalir(ir2, f, 2)
=#

tp = record() do
    a1 = f1(2)
    a2 = f2(a1)
    a3 = f3(a2)
end

@show tp
println("tp(2) = ", result(tp))
run(tp, 20)
println("tp(20) = ", result(tp))
