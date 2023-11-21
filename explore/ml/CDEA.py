import pandas
import torch
from torch import optim

class CDEA:
    def __init__(self, input="~/input.xlsx", output="~/output.xlsx"):
        self.input_file = input
        self.output_file = output
        self.epolicy = True
        self.policy_key = '试点与否'
        self.use_checkpoint = False

    def init_data(self):
        self.input_data = pandas.read_excel(self.input_file, sheet_name=None)
        self.output_data = pandas.read_excel(self.output_file, sheet_name=None)
        self.policy_data = self.input_data.pop(self.policy_key)

        self.n_input = len(self.input_data)
        self.keys_input = list(self.input_data.keys()) # names of sheets
        self.n_output = len(self.output_data)
        self.keys_output = list(self.output_data.keys()) # names of sheets

        df_input_0 = self.input_data[[*self.input_data.keys()][0]]
        # number of rows of the df
        self.n_regions = len(df_input_0)
        self.regions = df_input_0[df_input_0.columns[0]]
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
        self.E_ = torch.rand(self.n_regions, dtype=torch.float64, requires_grad=True)

        # self.restore_checkpoints_0()

    def restore_checkpoints_0(self):
        self.use_checkpoint = True
        # All Variables, Dynamic E, No TS
        self.V_ = torch.tensor([99.9, 99.9, -44.4387, 99.9], dtype=torch.float64, requires_grad=True)
        self.U_ = torch.tensor([-80.7511, -81.4838, 99.9, 99.9, 99.9, 99.9, -51.3516, -76.5833, -68.5186, 99.9, -71.8757, 99.9],
                               dtype=torch.float64, requires_grad=True)
        self.E_ = torch.tensor([-0.5671, -1.5475,  0.0599,  0.0413,  0.5225,  0.1637,  0.4996,  0.3229,
                                -0.4324,  0.6812,  0.4149,  0.7802, -0.7963,  0.1500,  0.5812,  0.9951,
                                -0.8254,  0.0269, -0.2335,  0.3756,  0.2750, -0.9915,  0.3336,  0.4737,
                                0.1214,  0.8314,  0.5448,  0.3247,  0.5720,  0.6419], dtype=torch.float64, requires_grad=True)

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
        if not self.epolicy:
            return 1
        year = self.years[y]
        p = self.policy_data[year][r]
        if p:
            return torch.sigmoid(self.E_[r])
        return 1

    def summary(self):
        with torch.no_grad():
            print("=== Technical Checkpoint ===")
            print(f"V = torch.{self.V_}")
            print(f"U = torch.{self.U_}")
            print(f"E = torch.{self.E_}")
            V = map(lambda x: x.item(), torch.sigmoid(self.V_))
            VS = list(zip(self.keys_input, V))
            U = map(lambda x: x.item(), torch.sigmoid(self.U_))
            US = list(zip(self.keys_output, U))

            print("=== Result ===")
            print(f"V = {VS}")
            print(f"U = {US}")
        print(self.productivity())

    def productivity(self):
        data = {}
        with torch.no_grad():
            sigmoid_v = torch.sigmoid(self.V_)
            sigmoid_u = torch.sigmoid(self.U_)
            sigmoid_e = torch.sigmoid(self.E_)

            real_e = []
            for r in range(self.n_regions):
                if any(self.policy_data.iloc[r][1:]):
                    real_e.append(sigmoid_e[r].item())
                else:
                    real_e.append(0)

            for r in range(self.n_regions):
                prods = []
                if self.epolicy:
                    prods.append(real_e[r])
                for y in range(self.n_years):
                    input_i = self.get_input(r, y)
                    XV = sigmoid_v * input_i
                    policy_i = self.get_policy(r, y)
                    XVE = torch.sum(policy_i * XV)

                    output_i = self.get_output(r, y)
                    YU = torch.dot(sigmoid_u, output_i)

                    H_i = YU / XVE
                    prods.append(H_i.item())
                data[self.regions[r]] = prods


        index = ["E"] + list(self.years) if self.epolicy else self.years
        return pandas.DataFrame(data, index=index).T

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
            if self.use_checkpoint:
                rate = 0.02
            else:
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
    engine.init_data()
    engine.train()
