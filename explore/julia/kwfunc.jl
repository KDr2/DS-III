
function dft_f0(j::Int, i::Int=0)
    print(j)
    print(i)
end

function kw_f0(j::Int, i::Int=0; karg0::String="abc", karg1::String="def")
    print(j)
    print(i)
    print(karg0)
    print(karg1)
end
