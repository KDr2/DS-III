library(readxl)

input.data = list()
for(i in seq(5)) input.data[[i]] <- data.frame(read_xlsx("input.xlsx", i))

output.data = list()
for(i in seq(12)) output.data[[i]] <- data.frame(read_xlsx("output.xlsx", i))

save(list(input=input.data, output=output.data), file="gd_data.Rdata")
