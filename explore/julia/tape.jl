using IRTools
using MacroTools

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
    buf = IOBuffer()
    print(buf, "$(length(tp))-element Tape")
    isempty(tp) || println(buf, ":")
    i = 1
    for instruction in tp
        print(buf, "\t$i => ")
        show(buf, instruction)
        i += 1
    end
    print(io, String(take!(buf)))
end

function (instr::Instruction{F})() where F
    output = instr.fun(map(val, instr.input)...)
    instr.output.val = output
end

function run(tape::Tape, args...)
    input = map(x-> box(x, tape), args)
    tape[1].input = input
    for instruction in tape
        instruction()
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

    # here we assumed the ir only has a return statement at its last block,
    # and we make sure the return value is from a function call (to `identity`)
    last_blk = IRTools.blocks(ir)[end]
    retv = IRTools.returnvalue(last_blk)
    IRTools.return!(last_blk, IRTools.xcall(Base, :identity, retv))

    for (x, st) in ir
        x == tape && continue
        Meta.isexpr(st.expr, :call) || continue
        ir[x] = IRTools.xcall(@__MODULE__, :run_and_record!, tape, st.expr.args...)
    end
    # the real return value will be in the last instruction on the tape
    IRTools.return!(ir, tape)
    return ir
end


IRTools.@dynamo function record(a...)
    ir = IRTools.IR(a...)
    return intercept(ir)
end


mutable struct TapedFunction
    func::Function
    ir::Union{Nothing, IRTools.IR}
    tape::Tape
    cur_idx::Int
    TapedFunction(f) = new(f, nothing, Instruction[], 0)
end

function (tf::TapedFunction)(args...)
    if isempty(tf.tape)
        ir = IRTools.@code_ir tf.func(args...)
        ir = intercept(ir)
        tape = IRTools.evalir(ir, tf.func, args...)
        tf.ir = ir
        tf.tape = tape
        return result(tape)
    else
        run(tf.tape, args...)
        return result(tf.tape)
    end
end

function Base.show(io::IO, tf::TapedFunction)
    buf = IOBuffer()
    println(buf, "TapedFunction:")
    println(buf, "  .func => $(tf.func)")
    println(buf, "  .ir   => $(tf.ir)")
    println(buf, "  .tape => $(tf.tape)")
    print(io, String(take!(buf)))
end

macro tapedfunction(expr)
    d = MacroTools.splitdef(expr)
    f = esc(d[:name])
    new_f_name = gensym(d[:name])
    d[:name] = new_f_name
    origin_func = MacroTools.combinedef(d)

    return quote
        $origin_func
        $f = TapedFunction($new_f_name)
    end
end

### Testing

f1(x) = x + 1
f2(x) = x * 2, x + 2
f3(x) = x * 3

@tapedfunction function t1(x)
    a1 = f1(x)
    a2, a3 = f2(a1)
    a2
end

@show t1
println()
@show t1(2)
println()
@show t1
println()
@show t1(3)
