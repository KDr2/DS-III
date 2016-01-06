

macro isdef_local(sym)
    sym::Symbol
    quote
        begin
            func = () -> $(sym)
            string(func.env[1]) != "Box(#undef)"
        end
    end
end


function test(b)
    if b
        v1 = 1
    else
        v2 = 1
    end
    (@isdef_local v1) && println("v1 is defined")
    (@isdef_local v2) && println("v2 is defined")
end


test(false)
test(true)
