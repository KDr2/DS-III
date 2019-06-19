using Flux
using Flux.Tracker

function limitation_delta(e)
    if abs(e) > 0 && abs(e) < 1
        (log(exp(8), e / (1 - e)))^2
    else
        1.0E1
    end
end

function loss(predict, X, Y)
    E = predict(X, Y)
    E2 = 1 .- E .+ limitation_delta.(E)
    return sum(E2)
end

mutable struct CDEA{T}
    X::Matrix{T}
    Y::Matrix{T}
    U::TrackedArray{T, 2}
    V::TrackedArray{T, 2}
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

    predict(x, y) = (sigmoid.(V) * y) ./ (sigmoid.(U) * x)

    CDEA{Float64}(x, y, U, V, predict)
end
create_cdea(x, y) = create_cdea(float.(x), float.(y))

function reset_cdea(model::CDEA)
    n_INPUT = size(model.X)[1]
    n_OUTPUT = size(model.Y)[1]

    U = rand(1, n_INPUT)
    V = rand(1, n_OUTPUT)
    U = param(U); V=param(V);
    model.U = U; model.V =V;
    
    predict(x, y) = (sigmoid.(V) * y) ./ (sigmoid.(U) * x)
    model.predict = predict

    model
end

function train(model::CDEA)
    last_loss_value = 1.0E8
    loop_count = 0
    while true
        loop_count += 1
        grads = Tracker.gradient(() -> loss(model.predict, model.X, model.Y), Flux.params(model.U, model.V))
        Tracker.update!(model.U, -0.1 * grads[model.U])
        Tracker.update!(model.V, -0.1 * grads[model.V])
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
end

function is_resloved(model::CDEA)
    E = model.predict(model.X, model.Y)
    all(E .<= 1.0) && all(E .>= 0)
end


function resovle(X, Y)
    results = Array{CDEA, 1}()
    while true
        m = create_cdea(X, Y)
        try train(m) catch end
        is_resloved(m) && push!(results, m)
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
Y1 = [ 60 22 24; 12 6 8]

X2 = [ 1 3 3 4; 3 1 3 2;]
Y2 = [ 1 1 2 1]

m = resovle(X1, Y1)
@show sigmoid.(m.U)
@show sigmoid.(m.V)
@show m.predict(m.X, m.Y)
