# Packages:
# Julia 1.5.3
# Flux = v0.11.6
# CSV = 0.8.3
# DataFrames = 0.22.5

using Logging

using CSV
using DataFrames
using Flux
using Flux.Optimise


log_level =  Logging.Debug # Logging.Info | Logging.Warn | Logging.Error
global_logger(SimpleLogger(stdout, log_level))

mutable struct CDEA{T}
    X::Matrix{T}
    Y::Matrix{T}
    V_::Array{T, 2}
    U_::Array{T, 2}
    maxH::Ref{Float64} # Max(H)
    predict::Function
end

function create_gd_dea(x::Matrix{Float64}, y::Matrix{Float64})
    size(x)[2] == size(y)[2] || throw("DMU Count Missmatch")
    n_DMU = size(x)[2]
    n_INPUT = size(x)[1]
    n_OUTPUT = size(y)[1]

    V_ = rand(1, n_INPUT)
    U_ = rand(1, n_OUTPUT)

    maxH = Ref(1.0)

    predict(x, y) = (sigmoid.(U_) * y) ./ (sigmoid.(V_) * x .* maxH[])

    CDEA{Float64}(x, y, V_, U_, maxH, predict)
end
create_gd_dea(x, y) = create_gd_dea(float.(x), float.(y))

function reset_gd_dea(model::CDEA)
    n_INPUT = size(model.X)[1]
    n_OUTPUT = size(model.Y)[1]

    V_ = rand(1, n_INPUT);
    U_ = rand(1, n_OUTPUT);
    model._U = _U; model._V =_V; model.maxH[] = 1.0;

    predict(x, y) = (sigmoid.(U_) * y) ./ (sigmoid.(V_) * x .* model.maxH[])
    model.predict = predict

    model
end

function loss(predict, X, Y)
    H = predict(X, Y)
    return sum((1 .- H) .^ 2)
end

function train(model::CDEA)
    last_loss_value = 1.0E8
    loop_count = 0
    opt = Descent(0.05)
    while true
        loop_count += 1
        grads = gradient(
            () -> loss(model.predict, model.X, model.Y),
            Flux.params(model.V_, model.U_)
        )
        # model.V_ .-= Flux.Optimise.apply!(opt, model.V_, grads[model.V_])
        # model.U_ .-= Flux.Optimise.apply!(opt, model.U_, grads[model.U_])
        Optimise.update!(opt, model.V_, grads[model.V_])
        Optimise.update!(opt, model.U_, grads[model.U_])
        loss_value = loss(model.predict, model.X, model.Y)
        if last_loss_value == loss_value || loop_count > 1.0E5
            break
        end
        last_loss_value = loss_value
        if loop_count % 20 == 0
            @show model.V_
            @show model.U_
            @show loss_value
        end
    end
    # normalize
    H = model.predict(model.X, model.Y)
    maxH = maximum(H)
    model.maxH[] = maxH
end

function resovle(X, Y)
    results = Array{CDEA, 1}()
    while true
        m = create_gd_dea(X, Y)
        train(m)
        push!(results, m)
        size(results)[1] >= 3 && break
    end
    H = map(results) do model
        sum(model.predict(model.X, model.Y))
    end
    results[argmax(H)]
end

function summary(m::CDEA)
    Dict{Symbol, Any}(
        :V => sigmoid.(m.V_) .* m.maxH[],
        :U => sigmoid.(m.U_),
        :H => m.predict(m.X, m.Y),
        :maxH => m.maxH[]
    )
end


##################
# RUN
##################

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

# m = resovle(X1, Y1)
# m = resovle(X2, Y2)
# m = resovle(X3, Y3)

input = CSV.read("/home/kdr2/wukong/data-20201106-input.txt", DataFrame)
XR = convert(Matrix, input[:, 2:end])'
output = CSV.read("/home/kdr2/wukong/data-20201106-output.txt", DataFrame)
YR = convert(Matrix, output[:, 2:end])'
@show XR
@show YR
m = resovle(XR, YR)

@info summary(m)
