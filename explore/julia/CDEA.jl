using Flux
using Flux.Tracker
using Flux.Optimise

inv_sigmoid(e) =  (e > 0 && e < 1) ? (log(exp(8), e / (1 - e)))^2 : 1.0E8

function loss(predict, X, Y)
    E = predict(X, Y)
    # E2 = 1 .- E .+ inv_sigmoid.(E)
    E2 = (1 .- E) .^ 2
    return sum(E2)
end

mutable struct CDEA{T}
    X::Matrix{T}
    Y::Matrix{T}
    U::TrackedArray{T, 2}
    V::TrackedArray{T, 2}
    eff_factor::Ref{Float64}
    predict::Function
end

function create_cdea(x::Matrix{Float64}, y::Matrix{Float64})
    size(x)[2] == size(y)[2] || throw("DMU Number Missmatch")
    n_DMU = size(x)[2]
    n_INPUT = size(x)[1]
    n_OUTPUT = size(y)[1]

    U = rand(1, n_INPUT)
    V = rand(1, n_OUTPUT)
    U = param(U); V=param(V);
    F = Ref(1.0)

    predict(x, y) = (sigmoid.(V) * y) ./ (sigmoid.(U) * x .* F[])

    CDEA{Float64}(x, y, U, V, F, predict)
end
create_cdea(x, y) = create_cdea(float.(x), float.(y))

function reset_cdea(model::CDEA)
    n_INPUT = size(model.X)[1]
    n_OUTPUT = size(model.Y)[1]

    U = rand(1, n_INPUT)
    V = rand(1, n_OUTPUT)
    U = param(U); V=param(V);
    model.U = U; model.V =V; model.eff_factor[] = 1.0;

    predict(x, y) = (sigmoid.(V) * y) ./ (sigmoid.(U) * x .* model.eff_factor[])
    model.predict = predict

    model
end

function train(model::CDEA)
    last_loss_value = 1.0E8
    loop_count = 0
    opt = Descent(0.1)
    while true
        loop_count += 1
        grads = Tracker.gradient(() -> loss(model.predict, model.X, model.Y), Flux.params(model.U, model.V))
        # Tracker.update!(model.U, -0.1 * grads[model.U])
        # Tracker.update!(model.V, -0.1 * grads[model.V])
        Tracker.update!(opt, model.U, grads[model.U])
        Tracker.update!(opt, model.V, grads[model.V])
        loss_value = loss(model.predict, model.X, model.Y)
        @show loop_count, loss_value
        if last_loss_value == loss_value || loop_count > 1.0E5
            break
        end
        last_loss_value = loss_value
        @show sigmoid.(model.U)
        @show sigmoid.(model.V)
        @show model.predict(model.X, model.Y)
    end

    @show last_loss_value
    @show sigmoid.(model.U)
    @show sigmoid.(model.V)
    @show model.predict(model.X, model.Y)

    # normalize
    E = model.predict(model.X, model.Y)
    maxE = maximum(E)
    model.eff_factor[] = maxE.data
end

function resovle(X, Y)
    results = Array{CDEA, 1}()
    while true
        m = create_cdea(X, Y)
        train(m)
        push!(results, m)
        size(results)[1] >= 3 && break
    end
    E = map(results) do model
        sum(model.predict(model.X, model.Y))
    end
    results[argmax(E)]
end

X1 = [4 15 27;
      15 4 5;
      8 2 5]
Y1 = [ 60 22 24;
       12 6 8]

X2 = [ 1 3 3 4;
       3 1 3 2;]
Y2 = [ 1 1 2 1]

# https://analyticsdefined.com/data-envelopment-analysis-in-r/
X3 = [51 38;
      60 45;
      43 33;
      53 43;
      43 38;
      44 35;]'
Y3 = [169 119;
      243 167;
      173 158;
      216 138;
      155 161;
      169 157;]'

m = resovle(X1, Y1)
# m = resovle(X2, Y2)
# m = resovle(X3, Y3)
@show sigmoid.(m.U)
@show sigmoid.(m.V)
@show m.predict(m.X, m.Y)
