## 4.4.5 样本总量确定——给定总费用情形

source("strata.cost.Tsize.r")

Nh=c(112, 68, 39)
S2h=c(2.25, 3.24, 3.24)
Ctotal=10000
Cfixed=1000
Ch=c(9, 25, 36)
allocation="Opt"

re4.4.5=strata.cost.Tsize(Nh, S2h, Ctotal, Cfixed, Ch, allocation)
print(re4.4.5)