import pandas
import torch
from torch import optim

class CDEA:
    def __init__(self, input="~/input.xlsx", output="~/output.xlsx",
                 policy_ingredients="~/policy_ingredients.xlsx"):
        self.input_file = input
        self.output_file = output
        self.policy_file = policy_ingredients
        self.epolicy = True
        self.policy_key = '试点与否'
        self.output_columns = []
        # self.output_columns = ['人均碳排放', '单位GDP碳排放'] # None for all
        # self.output_columns = ['工业总产值', 'GDP'] # None for all

    def init_data(self):
        # the data are dictionaries of sheets/dataframes
        self.input_data = pandas.read_excel(self.input_file, sheet_name=None)

        self.output_data = pandas.read_excel(self.output_file, sheet_name=None)
        if self.output_columns: # only pick some certain columns
            output_data = {}
            for k in self.output_columns:
                output_data[k] = self.output_data[k]
            self.output_data = output_data

        self.policy_data = self.input_data.pop(self.policy_key)
        self.policy_igd_data = pandas.read_excel(self.policy_file, sheet_name=None)

        self.n_input = len(self.input_data)
        self.keys_input = list(self.input_data.keys()) # names of sheets/input
        self.n_output = len(self.output_data)
        self.keys_output = list(self.output_data.keys()) # names of sheets/output
        self.n_policy_igd = len(self.policy_igd_data)
        self.keys_policy_igd = list(self.policy_igd_data.keys()) # names of sheets/policy-ingredients


        df_input_0 = self.input_data[[*self.input_data.keys()][0]]
        # number of rows of the df
        self.n_regions = len(df_input_0)
        self.regions = df_input_0[df_input_0.columns[0]]
        # eliminate the first column, which is the region name:
        self.n_years = len(df_input_0.columns) - 1
        self.years = df_input_0.columns[1:]

        # extract regions in policy data
        df_pi_0 = self.policy_igd_data[[*self.policy_igd_data.keys()][0]]
        self.p_regions = df_pi_0[df_pi_0.columns[0]]


        self.n_DMU = self.n_regions * self.n_years

        self.input_cache = {}
        self.output_cache = {}
        self.policy_cache = {}

    def init_parameters(self):
        self.V_ = torch.rand(self.n_input, dtype=torch.float64, requires_grad=True)
        self.U_ = torch.rand(self.n_output, dtype=torch.float64, requires_grad=True)
        self.Ke_ = torch.rand(self.n_policy_igd, dtype=torch.float64, requires_grad=True)
        self.VH = torch.tensor(0.3, requires_grad=True)

        self.opt = optim.Adam([self.V_, self.U_, self.Ke_, self.VH], lr=100)

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
            # here we use sum(K * ingrediants of policy) as e
            r_name = self.regions[r]
            rx = list(self.p_regions).index(r_name)
            data = []
            for pi in self.keys_policy_igd:
                pv = self.policy_igd_data[pi][year][rx]
                data.append(float(pv))
            ke = torch.sigmoid(self.Ke_)
            return torch.sum(ke * torch.tensor(data))
        return 1

    def loss(self):
        sigmoid_v = torch.sigmoid(self.V_)
        sigmoid_u = torch.sigmoid(self.U_)

        loss = torch.tensor(0.0, dtype=torch.float64)

        for r in range(self.n_regions):
            last_h = 0
            for y in range(self.n_years):
                input_i = self.get_input(r, y)
                XV = torch.sum(sigmoid_v * input_i)
                XV = XV + last_h * torch.sigmoid(self.VH)

                output_i = self.get_output(r, y)
                YU = torch.dot(sigmoid_u, output_i)

                policy_i = self.get_policy(r, y)

                H_i = policy_i * YU / XV
                last_h = H_i
                loss_i = torch.pow(1 - H_i, 2)
                if H_i > 1:
                    loss_i = loss_i * 10
                loss = loss + loss_i

        return loss

    def train(self):
        loop_count = 0

        self.init_parameters()
        self.summary()
        while True:
            loop_count += 1
            lv = self.loss()

            lrate = 0.2 if loop_count < 1000 else 0.1
            for g in self.opt.param_groups:
                g['lr'] = lrate

            if loop_count % 50 == 0:
                print(f"loop={loop_count}, lr={lrate}, loss={lv.item()}")
            if loop_count % 1000 == 0:
                self.summary()
            lv.backward()
            with torch.no_grad():
                self.opt.step()
                self.opt.zero_grad()

            if loop_count > 2e4:
                return lv.item() < self.n_DMU / 10

    def summary(self):
        with torch.no_grad():
            print("=== Technical Checkpoint ===")
            print(f" V = torch.{self.V_}")
            print(f" U = torch.{self.U_}")
            print(f" K = torch.{self.Ke_}")
            print(f"VH = {self.VH}")
            V = map(lambda x: x.item(), torch.sigmoid(self.V_))
            VS = list(zip(self.keys_input, V))
            U = map(lambda x: x.item(), torch.sigmoid(self.U_))
            US = list(zip(self.keys_output, U))

            print("=== Result ===")
            print(f"V = {VS}")
            print(f"U = {US}")

        p = self.productivity()
        p.to_csv("~/h.csv")
        e = self.policy_e()
        e.to_csv("~/e.csv")
        print("=== H of DMUs ===")
        print(p)
        print("=== E of Policies ===")
        print(e)

    def productivity(self):
        data = {}
        with torch.no_grad():
            sigmoid_v = torch.sigmoid(self.V_)
            sigmoid_u = torch.sigmoid(self.U_)

            last_h = 0
            for r in range(self.n_regions):
                prods = []
                for y in range(self.n_years):
                    input_i = self.get_input(r, y)
                    XV = torch.sum(sigmoid_v * input_i)
                    XV = XV + last_h * torch.sigmoid(self.VH)

                    output_i = self.get_output(r, y)
                    YU = torch.dot(sigmoid_u, output_i)

                    policy_i = self.get_policy(r, y)

                    H_i = policy_i * YU / XV
                    last_h = H_i
                    prods.append(round(H_i.item(), 2))
                data[self.regions[r]] = prods

        index = self.years
        return pandas.DataFrame(data, index=index).T

    def policy_e(self):
        data = {}
        with torch.no_grad():
            for r in range(self.n_regions):
                pe = []
                for y in range(self.n_years):
                    policy_i = self.get_policy(r, y)
                    if policy_i == 1:
                        policy_i = torch.tensor(0)

                    pe.append(round(policy_i.item(), 2))
                data[self.regions[r]] = pe

        index = self.years
        return pandas.DataFrame(data, index=index).T


if __name__ == '__main__':
    engine = CDEA()
    engine.init_data()
    engine.train()
