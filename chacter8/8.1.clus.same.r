##8.1群大小相等
source("clus.r")
source("conf_interval.R")

N=512
n=12
M=8
yi=c(188, 180.5, 149.75, 207.875, 244.25, 278.50,
     182.75, 211.50, 253.125, 191.125, 274.75, 258.375)*M
alpha=0.05

re=clus.samesize(N=N, n=n, M=M, yi=yi, alpha=alpha)
print(re)