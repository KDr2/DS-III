xc = require("xcorot")

function lf1()
    print("Lua:enter lf1")
    local x=xc.cf1("lf2")
    print("CF1:".. tostring(x))
end

function lf2(n)
    print("enter lf2")
    local x= coroutine.yield(1)
    print("leave lf2" .. tostring(x))
    return 1
end


co = coroutine.create(lf1)
v,v1 = coroutine.resume(co)
print("xxx:" .. tostring(v1) .. ", " .. tostring(coroutine.status(co)))
v = coroutine.resume(co, 234)

print("yyy:" .. tostring(v))
