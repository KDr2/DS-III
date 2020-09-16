# Generate SVG FlameGraphs using
# https://github.com/timholy/FlameGraphs.jl and
# https://github.com/brendangregg/FlameGraph
# [deps]
# FlameGraphs = "08572546-2f56-4bcf-ba4e-bab62c3a3f89"
# Profile = "9abbd945-dff8-562f-b5e8-e1ebf5ef1b79"


module FlameSVG

using Profile
using FlameGraphs

export @flamesvg

const PROJECT_ROOT = dirname(@__FILE__) |> dirname |> abspath
const PERL_SCRIPT = joinpath(PROJECT_ROOT, "deps", "flamegraph.pl")

@static if VERSION < v"1.3.0"
    Base.string(::Nothing) = "nothing"
end

function stringify(node::FlameGraphs.NodeData; prefix="", as_prefix=false)
    tail = as_prefix ? ";" : " $(length(node.span))"
    method = replace(node.sf.linfo |> string, "MethodInstance for " => "")
    file_path = replace(node.sf.file |> string, r"^.*\.julia/packages" => "{DEPOT}")
    "$(prefix)$(method)@$(file_path):$(node.sf.line)$(tail)"
end

function stackcollapse(tree; prefix="", output=stdout)
    node = tree.data

    println(output, stringify(node, prefix=prefix))

    prefix = stringify(node, prefix=prefix, as_prefix=true)
    for child in tree
        stackcollapse(child, prefix=prefix, output=output)
    end
end

function flamegraph_to_svg(name)
    g = flamegraph()
    folded = tempname()
    open(folded, "w") do out
        stackcollapse(g, output=out)
    end
    run(pipeline(`perl $(PERL_SCRIPT) $(folded)`, "$(name).svg"))
end

macro flamesvg(name, expr)
    quote
        Profile.clear();
        Profile.@profile $(esc(expr))
        flamegraph_to_svg($(esc(name)))
    end
end

end

# Testing

"""
using Test
using FlameSVG


@testset "Generate an SVG" begin
    function profile_test(n)
        for i = 1:n
            A = randn(100,100,20)
            m = maximum(A)
            Am = mapslices(sum, A; dims=2)
            B = A[:,:,5]
            Bsort = mapslices(sort, B; dims=1)
            b = rand(100)
            C = B.*b
        end
    end

    name = "test-case-1"
    @test !isfile(name * ".svg")
    @flamesvg name profile_test(10)
    @test isfile(name * ".svg")
end
"""
