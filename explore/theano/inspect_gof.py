import theano
from theano import tensor

x1 = tensor.dscalar("x1")
x2 = tensor.dscalar("x2")
y = x1 + x2
f = theano.function([x1, x2], [y])
