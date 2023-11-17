import pandas
import torch
from torch import optim

class CDEA:
    def __init__(self):
        self.policy_key = '试点与否'

        self.input_data = pandas.read_excel("~/input.xlsx", sheet_name=None)
        self.output_data = pandas.read_excel("~/output.xlsx", sheet_name=None)
        self.policy_data = self.input_data.pop(self.policy_key)

        self.n_input = len(self.input_data)
        self.keys_input = list(self.input_data.keys()) # names of sheets
        self.n_output = len(self.output_data)
        self.keys_output = list(self.output_data.keys()) # names of sheets

        df_input_0 = self.input_data[[*self.input_data.keys()][0]]
        # number of rows of the df
        self.n_regions = len(df_input_0)
        # eliminate the first column, which is the region name:
        self.n_years = len(df_input_0.columns) - 1
        self.years = df_input_0.columns[1:]

        self.n_DMU = self.n_regions * self.n_years

        self.input_cache = {}
        self.output_cache = {}
        self.policy_cache = {}

    def init_parameters(self):
        self.V_ = torch.rand(self.n_input, dtype=torch.float64, requires_grad=True)
        self.U_ = torch.rand(self.n_output, dtype=torch.float64, requires_grad=True)
        self.E_ = torch.tensor(1.0, dtype=torch.float64, requires_grad=True)

    def get_input(self, r, y):
        if (r, y) in self.input_cache:
            return self.input_cache[(r, y)]
        year = self.years[y]
        data = []
        for iname in self.keys_input:
            data.append(self.input_data[iname][year][r])
        ret = torch.tensor(data)
        self.input_cache[(r, y)] = ret
        return ret

    def get_output(self, r, y):
        if (r, y) in self.output_cache:
            return self.output_cache[(r, y)]
        year = self.years[y]
        data = []
        for oname in self.keys_output:
            data.append(self.output_data[oname][year][r])
        ret = torch.tensor(data)
        self.output_cache[(r, y)] = ret
        return ret

    def get_policy(self, r, y):
        return 1
        year = self.years[y]
        p = self.policy_data[year][r]
        if p:
            return torch.sigmoid(self.E_)
        return 1

    def summary(self):
        with torch.no_grad():
            V = torch.sigmoid(self.V_)
            U = torch.sigmoid(self.U_)
            E = 1 - torch.sigmoid(self.E_)
            print(f"V = {V}")
            print(f"U = {U}")
            print(f"E = {E}")

    def loss(self):
        sigmoid_v = torch.sigmoid(self.V_)
        sigmoid_u = torch.sigmoid(self.U_)

        loss = torch.tensor(0.0, dtype=torch.float64)

        for r in range(self.n_regions):
            for y in range(self.n_years):
                input_i = self.get_input(r, y)
                XV = sigmoid_v * input_i
                policy_i = self.get_policy(r, y)
                XVE = torch.sum(policy_i * XV)

                output_i = self.get_output(r, y)
                YU = torch.dot(sigmoid_u, output_i)

                H_i = YU / XVE

                loss_i = torch.pow(1 - H_i, 2)
                loss = loss + loss_i

        return loss

    def train(self):
        # last_loss_val = -1
        loop_count = 0

        self.init_parameters()
        self.summary()
        while True:
            loop_count += 1
            lv = self.loss()
            # / math.log(loop_count + 10, 10)
            rate = 100.0 / loop_count if loop_count < 1000 else 0.05

            if loop_count % 50 == 0:
                print(loop_count, rate, lv)
            if loop_count % 1000 == 0:
                self.summary()
            lv.backward()
            with torch.no_grad():
                opt = optim.Adam([self.V_, self.U_, self.E_], lr=rate)
                opt.step()
                opt.zero_grad()
                # self.V_ -= rate * self.V_.grad
                # self.U_ -= rate * self.U_.grad
                self.V_.grad = None
                self.U_.grad = None
                self.E_.grad = None

            if loop_count > 1e8:
                break


if __name__ == '__main__':
    engine = CDEA()
    engine.train()
