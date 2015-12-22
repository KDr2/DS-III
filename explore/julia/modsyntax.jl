
module Outer

module Inner

module Norm

var = 1

module Runner

import ..Norm
f() = print(Norm.var)

end

end

end

end

Outer.Inner.Norm.Runner.f()
