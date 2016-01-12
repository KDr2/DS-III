

macro isdef_local(sym::Symbol)
    quote
        begin
            func = () -> $(esc(sym))
            length(func.env) == 1 && string(func.env[1]) != "Box(#undef)"
        end
    end
end


function test_0()
    (@isdef_local v1) && println("v1 is defined")
end

function test_1(b)
    if b
        v1 = 1
    else
        v2 = 1
    end
    (@isdef_local v1) && println("v1 is defined")
    (@isdef_local v2) && println("v2 is defined")
end

test_0()
test_1(false)
test_1(true)
