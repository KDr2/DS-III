import math

import torch
from torch import optim
import pandas as pd

class GDDEA:
    def __init__(self, X, Y):
        self.X = X
        self.Y = Y

        self.n_DMU = self.X.size()[1]
        self.n_INPUT = self.X.size()[0]
        self.n_OUTPUT = self.Y.size()[0]

        self.V_ = torch.rand(self.n_DMU, self.n_INPUT, requires_grad=True)
        self.U_ = torch.rand(self.n_DMU, self.n_OUTPUT, requires_grad=True)

        torch.nn.init.normal_(self.V_)
        torch.nn.init.normal_(self.U_)

    def loss(self, log=False):
        H_sum = 0
        constraints = 0
        for i in range(self.n_DMU):
            for j in range(self.n_DMU):
                X_i = torch.dot(torch.abs(self.V_[i, :]), self.X[:, j])
                Y_i = torch.dot(torch.abs(self.U_[i, :]), self.Y[:, j])
                H_i = Y_i / X_i
                # loss 1: H -> 1 when i == j
                if i == j:
                    H_sum += (H_i - 1).pow(2)
                # else:
                #    H_sum += (H_i - 0).pow(2)
                # end if

                # loss 2: all H in [0, 1]
                constraints +=  torch.max(torch.tensor(0, dtype=torch.float), (2 * H_i - 1).pow(2) - 1)

        # H_sum *= (self.n_DMU - 1)
        loss_val =  H_sum + constraints
        if log:
            with torch.no_grad():
                print(loss_val, "=", H_sum, "+", constraints)
        return loss_val

    def train(self):
        last_loss_val = -1
        loop_count = 0
        rate = 50.0 # / math.log(loop_count + 10, 10)

        while True:
            loop_count += 1
            lv = self.loss(log=loop_count % 50 == 0)

            if loop_count % 50 == 0:
                print(loop_count, rate, lv.item())
            if loop_count % 1000 == 0:
                self.summary()
            lv.backward()
            with torch.no_grad():
                opt = optim.Adam([self.V_, self.U_], lr=rate)
                opt.step()
                opt.zero_grad()
                # self.V_ -= rate * self.V_.grad
                # self.U_ -= rate * self.U_.grad
                self.V_.grad = None
                self.U_.grad = None

            if loop_count > 1e8:
                break


    def predict(self):
        H = []
        with torch.no_grad():
            for i in range(self.n_DMU):
                X_i = torch.dot(torch.abs(self.V_[i, :]), self.X[:, i])
                Y_i = torch.dot(torch.abs(self.U_[i, :]), self.Y[:, i])
                H_i = Y_i / X_i
                H.append(H_i.item())
        return H

    def ematrix(self):
        st = [[] for _ in range(self.n_DMU)]
        with torch.no_grad():
            for i in range(self.n_DMU):
                for j in range(self.n_DMU):
                    X_i = torch.dot(torch.abs(self.V_[i, :]), self.X[:, j])
                    Y_i = torch.dot(torch.abs(self.U_[i, :]), self.Y[:, j])
                    H_i = Y_i / X_i
                    st[i].append(H_i.item())
        return st

    def summary(self):
        # print(self.U_)
        # print(self.V_)
        print(pd.DataFrame(self.ematrix()))
        print(self.predict())


###
X1 = torch.tensor(pd.read_csv("input.csv", delimiter="\t").values, dtype=torch.float).t()
Y1 = torch.tensor(pd.read_csv("output.csv", delimiter="\t").values, dtype=torch.float).t()
m = GDDEA(X1, Y1)

m.train()
m.summary()
